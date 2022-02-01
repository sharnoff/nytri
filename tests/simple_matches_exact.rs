use nytri::Regex;
use std::panic::{catch_unwind, resume_unwind};

#[rustfmt::skip]
static TEST_CASES: &[(&str, &[(bool, &[&str])])] = &[
    ("()", &[
        (true, &[""]),
        (false, &["ab"]),
    ]),
    ("a(|)b", &[
        (true, &["ab"]),
        (false, &[
            "aab",
            "abb",
            "ba",
            "abab",
        ]),
    ]),
    ("a|bc", &[
        (true, &[
            "a",
            "bc",
        ]),
        (false, &[
            "ab",
            "abc",
        ]),
    ]),
    ("ab?c", &[
        (true, &[
            "abc",
            "ac",
        ]),
        (false, &[
            "ab",
            "bc",
            "abbc",
        ]),
    ]),
    ("a(b|c)*d", &[
        (true, &[
            "ad",
            "abd",
            "acbd",
        ]),
        (false, &[
            "acc",
        ]),
    ]),
    ("a(b|c)+d", &[
        (true, &[
            "abd",
            "acbd",
        ]),
        (false, &[
            "ad",
            "acc",
        ]),
    ]),
];

#[test]
fn simple_matches_exact() {
    for &(pat, cases) in TEST_CASES {
        let re =
            Regex::new(pat).expect(&format!("could not construct regex for pattern {:?}", pat));

        for &(should_match, inputs) in cases {
            for input in inputs {
                let did_match = match catch_unwind(|| re.matcher().feed(input).is_match()) {
                    Err(e) => {
                        eprintln!("matching panicked for pattern {pat:?} on input {input:?}");
                        resume_unwind(e)
                    }
                    Ok(result) => result,
                };

                assert!(
                    did_match == should_match,
                    "unexpected resulting matching {:?} for input {:?}, expected is_match = {:?}, got {:?}",
                    pat, input, should_match, did_match,
                );
            }
        }
    }
}
