use smallvec::SmallVec;
use std::collections::HashMap;
use std::fmt;
use std::io;
use std::path::Path;
use std::sync::atomic::{AtomicUsize, Ordering};

mod test;

static CACHE_HITS: AtomicUsize = AtomicUsize::new(0);

#[derive(Copy, Clone, PartialEq, Eq, Debug, PartialOrd, Ord)]
struct Word([char; 5]);

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
enum GuessResultLetter {
    Black,
    Amber,
    Green,
}

type GuessResult = [GuessResultLetter; 5];

type SmallIndex = u8;

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
enum LetterState {
    Unknown,
    Known(SmallIndex),
}

type UnplacedLetter = (SmallIndex, u8);

#[derive(Clone, PartialEq, Eq, Hash)]
struct State {
    excluded: u32,
    letters: [LetterState; 5],
    unplaced: SmallVec<[UnplacedLetter; 5]>,
}

impl State {
    fn new() -> Self {
        Self {
            excluded: 0,
            letters: [LetterState::Unknown; 5],
            unplaced: SmallVec::new(),
        }
    }
}

impl fmt::Display for Word {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0
            .iter()
            .map(|c| write!(f, "{}", c.to_uppercase()))
            .collect()
    }
}

fn word(src: &str) -> Word {
    assert_eq!(src.len(), 5, "{} is not a valid word", src);
    let mut result: [char; 5] = [0 as char; 5];
    for (x, p) in src.chars().zip(result.iter_mut()) {
        *p = x;
    }
    Word(result)
}

fn index(c: char) -> usize {
    c as usize - 'a' as usize
}

fn deindex(i: usize) -> char {
    (i + 'a' as usize) as u8 as char
}

fn read_answers() -> Vec<Word> {
    read_words("answers.txt").unwrap()
}

fn read_allowed() -> Vec<Word> {
    read_words("allowed.txt").unwrap()
}

fn read_words<P>(filename: P) -> io::Result<Vec<Word>>
where
    P: AsRef<Path>,
{
    use io::BufRead;
    use std::fs::File;
    let file = File::open(filename)?;
    io::BufReader::new(file).lines().map(read_word).collect()
}

fn read_word(w: io::Result<String>) -> io::Result<Word> {
    let string: String = w?;
    if string.len() != 5 {
        use std::io::ErrorKind;
        return Err(ErrorKind::InvalidData.into());
    }
    let mut result: [char; 5] = [0 as char; 5];
    for (x, p) in string.chars().zip(result.iter_mut()) {
        *p = x;
    }
    return Ok(Word(result));
}

fn evaluate_guess(guess: &Word, target: &Word) -> GuessResult {
    let mut result = [GuessResultLetter::Black; 5];
    let mut ambers_avail = [0; 26];
    for ii in 0..5 {
        if guess.0[ii] == target.0[ii] {
            result[ii] = GuessResultLetter::Green;
        } else {
            ambers_avail[index(target.0[ii])] += 1;
        }
    }
    for ii in 0..5 {
        if result[ii] != GuessResultLetter::Green && ambers_avail[index(guess.0[ii])] > 0 {
            result[ii] = GuessResultLetter::Amber;
            ambers_avail[index(guess.0[ii])] -= 1;
        }
    }
    result
}

fn display_guess_result(result: &GuessResult) {
    for letter in result {
        print!(
            "{}",
            match letter {
                GuessResultLetter::Green => "G",
                GuessResultLetter::Amber => "A",
                _ => "x",
            }
        );
    }
}

fn display_guess(guess: &Word, target: &Word) {
    print!("Guessing {} with target word {}: result is ", guess, target);
    display_guess_result(&evaluate_guess(guess, target));
    println!(".");
}

fn display_state(state: &State) {
    print!("Current word: ");
    for ii in 0..5 {
        print!(
            "{}",
            match state.letters[ii] {
                LetterState::Known(n) => deindex(n as usize),
                LetterState::Unknown => '.',
            }
            .to_uppercase()
        )
    }
    println!();
    print!("Letters excluded: ");
    for ii in 0..26 {
        if state.excluded & (1 << ii) != 0 {
            print!("{}", deindex(ii).to_uppercase())
        }
    }
    println!();
    if state.unplaced.len() > 0 {
        println!("Other unplaced letters:");
        for (si, bits) in state.unplaced.iter() {
            print!(
                "{}, possible positions ",
                deindex(*si as usize).to_uppercase()
            );
            for ii in 0..5 {
                if bits & (1 << ii) != 0 {
                    print!("?");
                } else {
                    print!(".")
                }
            }
            println!();
        }
    }
}

fn update_state(s: &mut State, guess: &Word, result: GuessResult) {
    let mut new_green = [0; 26];
    let mut green_mask = 0u8;
    // mark our greens first
    for ii in 0..5 {
        if result[ii] == GuessResultLetter::Green {
            if s.letters[ii] == LetterState::Unknown {
                let idx = index(guess.0[ii]);
                s.letters[ii] = LetterState::Known(idx as u8);
                new_green[idx] += 1;
            }
        }
        if let LetterState::Known(_) = s.letters[ii] {
            green_mask |= 1 << ii;
        }
    }
    // for each new green, delete one (but only one) instance of that letter from the unplaced list
    // also delete the location of any greens as possible places for unplaced letters
    let mut curr_unplaced = 0;
    while curr_unplaced < s.unplaced.len() {
        if new_green[s.unplaced[curr_unplaced].0 as usize] > 0 {
            new_green[s.unplaced[curr_unplaced].0 as usize] -= 1;
            s.unplaced.remove(curr_unplaced);
        } else {
            s.unplaced[curr_unplaced].1 &= !green_mask;
            curr_unplaced += 1;
        }
    }
    // now work out if we found any new ambers
    // don't worry about the positions, we'll sort those out in a moment
    let mut known_count = [0; 26];
    for (idx, _) in s.unplaced.iter() {
        known_count[*idx as usize] += 1;
    }
    for ii in 0..5 {
        if let LetterState::Known(idx) = s.letters[ii] {
            known_count[idx as usize] += 1
        }
    }
    let mut seen_count = [0; 26];
    for ii in 0..5 {
        let idx = index(guess.0[ii]);
        match result[ii] {
            GuessResultLetter::Green | GuessResultLetter::Amber => {
                seen_count[idx] += 1;
                if seen_count[idx] > known_count[idx as usize] {
                    let mut positions = 0b11111 & !green_mask;
                    for (cidx, cp) in s.unplaced.iter() {
                        if idx == *cidx as usize {
                            positions = *cp;
                        }
                    }
                    s.unplaced.push((idx as u8, positions));
                }
            }
            GuessResultLetter::Black => {
                //if seen_count[idx] == 0 && known_count[idx] == 0 {
                s.excluded |= 1 << idx;
                //}
            }
        }
    }
    // final thing we have to do is update positions of any ambers
    for ii in 0..5 {
        if result[ii] == GuessResultLetter::Amber {
            for (idx, p) in s.unplaced.iter_mut() {
                if index(guess.0[ii]) as u8 == *idx {
                    *p &= !(1 << ii);
                }
            }
        }
    }
}

fn admits(state: &State, word: &Word) -> bool {
    let mut known = [0; 26];
    let mut seen = [0; 26];
    for ii in 0..5 {
        let l = index(word.0[ii]) as u8;
        seen[l as usize] += 1;
        if let LetterState::Known(k) = state.letters[ii] {
            if k != l {
                return false;
            }
            known[k as usize] += 1;
        }
    }
    let mut up_counted = [false; 5];
    for (idx, posns) in &state.unplaced {
        known[*idx as usize] += 1;
        let mut ok = false;
        for ii in 0..5 {
            if index(word.0[ii]) as u8 == *idx
                && posns & (1 << ii) == 0
                && state.letters[ii] == LetterState::Unknown
            {
                return false;
            }
        }
        for ii in 0..5 {
            if index(word.0[ii]) as u8 == *idx && posns & (1 << ii) != 0 && !up_counted[ii] {
                ok = true;
                up_counted[ii] = true;
                break;
            }
        }
        if !ok {
            return false;
        }
    }
    for idx in 0..26 {
        if state.excluded & (1 << idx) != 0 {
            if seen[idx] > known[idx] {
                return false;
            }
        }
    }
    true
}

fn entropy(state: &State, guess: &Word, answers: &Vec<Word>) -> f32 {
    let mut freq = HashMap::<GuessResult, u32>::new();
    let mut tot = 0.0;
    for w in answers {
        if admits(state, w) {
            let result = evaluate_guess(&guess, &w);
            if !freq.contains_key(&result) {
                freq.insert(result, 0);
            }
            *freq.get_mut(&result).unwrap() += 1;
            tot += 1.0;
        }
    }
    let mut entropy = 0.0;
    for result in freq.keys() {
        let p = freq[result] as f32 / tot;
        let e = p * -p.log2();
        entropy += e;
    }
    entropy
}

fn expected_turns(
    state: &State,
    answers: &Vec<Word>,
    cache: &mut HashMap<State, f32>,
    verbose: bool,
) -> f32 {
    if let Some(r) = cache.get(state) {
        CACHE_HITS.fetch_add(1, Ordering::SeqCst);
        return *r;
    }
    let options: Vec<&Word> = answers.iter().filter(|a| admits(state, a)).collect();
    if options.len() == 0 {
        panic!("ended up in an impossible state");
    }
    if options.len() == 1 {
        return 1.0;
    }
    let mut best = f32::INFINITY;
    let mut best_o = None;
    if verbose {
        println!("Exploring {} options", options.len());
    }
    let mut count = 0;
    for o in &options {
        let t = expected_turns_for_guess(state, o, answers, &options, cache);
        if t < best {
            best = t;
            best_o = Some(o);
        }
        count += 1;
        /*if verbose {
            println!("{} explored", count);
        }*/
    }
    if verbose {
        println!(
            "Best option was {} with {} expected guesses",
            best_o.unwrap(),
            best
        );
    }
    cache.insert(state.clone(), best);
    /*if cache.len() % 1000 == 0 {
        let h = CACHE_HITS.load(Ordering::SeqCst) as f32;
        println!(
            "cache size is {}, hit rate = {}",
            cache.len(),
            (h / (h + cache.len() as f32))
        );
    }*/
    best
}

fn expected_turns_for_guess(
    state: &State,
    guess: &Word,
    answers: &Vec<Word>,
    possibles: &Vec<&Word>,
    cache: &mut HashMap<State, f32>,
) -> f32 {
    let mut avg = 0.0;
    let denom = 1.0 / (possibles.len() as f32);
    for p in possibles {
        if *p != guess {
            let mut ns = state.clone();
            let gr = evaluate_guess(guess, p);
            update_state(&mut ns, guess, gr);
            avg += expected_turns(&ns, answers, cache, false) * denom;
        }
    }
    1.0 + avg
}

fn main() {
    let secret = word("taunt");
    let answers = read_answers();
    let allowed = read_allowed();

    let mut s = State::new();
    let guess = word("raise");
    let gr = evaluate_guess(&guess, &secret);
    display_guess(&guess, &secret);
    update_state(&mut s, &guess, gr);
    display_state(&s);

    println!("Possible words:");
    for w in &answers {
        if admits(&s, &w) {
            print!("{} ", w);
        }
    }
    println!();

    let mut best_guesses: Vec<(u32, &Word)> = answers
        .iter()
        .chain(allowed.iter())
        //.filter(|w| admits(&s, &w)) // hard mode
        .map(|w| ((entropy(&s, &w, &answers) * 1000000.) as u32, w))
        .collect();
    best_guesses.sort();
    let (e, top) = best_guesses.last().unwrap();
    println!("Best guess is {} (entropy score={})", top, e);

    let gr = evaluate_guess(&top, &secret);
    display_guess(&top, &secret);
    update_state(&mut s, &top, gr);
    display_state(&s);

    println!("Possible words:");
    for w in &answers {
        if admits(&s, &w) {
            print!("{} ", w);
        }
    }
    println!();

    let mut cache = HashMap::new();
    // from here on we explore the full game tree (hard mode)
    println!(
        "Overall expected turns to win: {}",
        expected_turns(&s, &answers, &mut cache, true) + 2.0
    );
}
