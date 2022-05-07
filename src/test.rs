#[cfg(test)]
use super::*;

#[test]
fn memory_size() {
    assert_eq!(std::mem::size_of::<State>(), 48);
}

#[test]
fn num_answers() {
    assert_eq!(read_answers().len(), 2309);
}

#[test]
fn num_allowed() {
    assert_eq!(read_allowed().len(), 10638);
}

#[test]
fn trash() {
    let secret = word("trash");
    let mut s = State::new();
    let guess = word("neath");
    let gr = evaluate_guess(&guess, &secret);
    update_state(&mut s, &guess, gr);
    assert!(admits(&s, &word("trash")));
    assert!(!admits(&s, &word("train")));
}

#[test]
fn river() {
    let secret = word("river");
    let mut s = State::new();
    let guess = word("error");
    let gr = evaluate_guess(&guess, &secret);
    update_state(&mut s, &guess, gr);
    assert!(admits(&s, &word("river")));
    assert!(!admits(&s, &word("after")));
    assert!(!admits(&s, &word("rarer")));
}

#[cfg(test)]
fn test_sequence(secret: Word, guesses: Vec<Word>) {
    let answers = read_answers();
    let mut expected = answers.clone();
    let mut state = State::new();
    for g in guesses {
        let result = evaluate_guess(&g, &secret);
        expected.retain(|ans| evaluate_guess(&g, &ans) == result);
        update_state(&mut state, &g, result);
        let actual: Vec<Word> = answers
            .iter()
            .filter(|ans| admits(&state, ans))
            .cloned()
            .collect();
        for a in actual.iter() {
            assert!(
                expected.as_slice().contains(&a),
                "state admits {} but shouldn't",
                a
            );
        }
        for e in expected.iter() {
            assert!(
                actual.as_slice().contains(&e),
                "state should admit {} but doesn't",
                e
            );
        }
    }
}

#[test]
fn larva() {
    test_sequence(
        word("larva"),
        vec![word("neath"), word("mails"), word("largo")],
    );
}

#[test]
fn melee() {
    test_sequence(
        word("melee"),
        vec![word("neath"), word("fever"), word("melee")],
    );
}

#[test]
fn sweet() {
    test_sequence(
        word("sweet"),
        vec![
            word("tepid"),
            word("chink"),
            word("winch"),
            word("renew"),
            word("swiss"),
        ],
    );
}
