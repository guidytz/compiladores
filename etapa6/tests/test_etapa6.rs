#[cfg(test)]
mod test {
    use etapa6::get_new_label;
    // use etapa6::get_new_temp;

    #[test]
    fn test_get_temp() {
        // for i in 0..10 {
        //     let exp_temp = format!("r{i}");
        //     let temp = get_new_temp().unwrap();
        //     assert_eq!(
        //         exp_temp, temp,
        //         "Temporary values are note the same! Expected: {exp_temp}. Got: {temp}"
        //     );
        // }
    }

    #[test]
    fn test_get_label() {
        for i in 0..20 {
            let exp_label = format!(".L{i}");
            let label = get_new_label();
            assert_eq!(
                exp_label, label,
                "Label values are note the same! Expected: {exp_label}. Got: {label}"
            );
        }
    }
}
