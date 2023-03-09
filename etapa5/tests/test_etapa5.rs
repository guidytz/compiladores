#[cfg(test)]
mod test {
    use etapa5::{get_new_label, get_new_temp};

    #[test]
    fn test_get_temp() {
        for i in 0..20 {
            let exp_temp = format!("r{i}");
            let temp = get_new_temp();
            assert_eq!(
                exp_temp, temp,
                "Temporary values are note the same! Expected: {exp_temp}. Got: {temp}"
            );
        }
    }

    #[test]
    fn test_get_label() {
        for i in 0..20 {
            let exp_label = format!("L{i}");
            let label = get_new_label();
            assert_eq!(
                exp_label, label,
                "Label values are note the same! Expected: {exp_label}. Got: {label}"
            );
        }
    }
}
