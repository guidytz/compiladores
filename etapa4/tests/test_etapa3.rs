#[cfg(test)]
mod test {
    use std::collections::{HashMap, HashSet};

    use etapa4::SCOPE_STACK;
    use lrlex::lrlex_mod;
    use lrpar::lrpar_mod;

    lrlex_mod!("scanner.l");
    lrpar_mod!("parser.y");

    #[test]
    fn test_etapa3() {
        let inputs =
            std::fs::read_dir("../test_data/etapa3/inputs").expect("Could not read input dir");

        for input in inputs {
            match input {
                Ok(input) => {
                    let input_file_name = input
                        .file_name()
                        .to_str()
                        .expect("Couldn't parse input file name")
                        .to_owned();

                    let input = std::fs::read_to_string(input.path())
                        .expect(format!("Couldn't read file {}", input_file_name).as_str());

                    let output_file_path =
                        format!("../test_data/etapa3/outputs/{input_file_name}.ref.dot");

                    println!("{input_file_name}");

                    let expected_result = std::fs::read_to_string(output_file_path)
                        .expect("Couldn't read expected output file");

                    SCOPE_STACK.with(|stack| stack.borrow_mut().new_scope());

                    let lexerdef = scanner_l::lexerdef();
                    let lexer = lexerdef.lexer(&input);
                    let (tree, _) = parser_y::parse(&lexer);

                    if let Ok(tree) = tree.unwrap() {
                        let tree_output = tree.to_string(&lexer);

                        if !compare(tree_output.clone(), expected_result.clone()) {
                            eprintln!("File {input_file_name} gave different results!");
                            eprintln!("expected: {}", normalize_graph(expected_result));
                            eprintln!("got: {tree_output}");
                            panic!("Different trees!");
                        }
                    }
                }
                _ => panic!("Something went wrong with entry dirs"),
            }
        }
    }

    fn normalize_graph(graph_string: String) -> String {
        graph_string
            .split("\n")
            .filter(|line| !(line.contains("digraph") || line.contains("Ref") || *line == "}"))
            .map(|line| line.replace(" ", ""))
            .map(|line| line.replace("->", ","))
            .collect::<Vec<_>>()
            .join("\n")
    }

    fn parse(tree_string: String) -> (HashMap<String, String>, HashMap<String, Vec<String>>) {
        let mut labels = HashMap::new();
        let mut tree = HashMap::new();
        tree_string
            .split("\n")
            .filter(|line| line.contains("label"))
            .for_each(|line| {
                let id_label = line.split("[").collect::<Vec<_>>();
                let id = id_label
                    .first()
                    .expect("couldn't get id")
                    .to_string()
                    .replace(" ", "");
                let label = "[".to_string() + id_label.last().expect("couldn't get label");
                labels.insert(id, label.replace(" ", ""));
            });

        tree_string
            .split("\n")
            .filter(|line| !line.contains("label"))
            .for_each(|line| {
                if line.is_empty() {
                    return ();
                }
                let parent_child = line.split(",").collect::<Vec<_>>();

                let parent = parent_child
                    .first()
                    .expect("couldn't get parent")
                    .to_string()
                    .replace(" ", "");

                let child = parent_child
                    .last()
                    .expect("couldn't get child")
                    .to_string()
                    .replace(" ", "");

                tree.entry(parent)
                    .and_modify(|children: &mut Vec<String>| children.push(child.clone()))
                    .or_insert(vec![child]);
            });

        (labels, tree)
    }

    fn find_root(tree: &HashMap<String, Vec<String>>) -> Option<String> {
        let mut node_set = HashSet::new();
        for (_, children) in tree {
            node_set.extend(children);
        }
        for (parent, _) in tree {
            if !node_set.contains(parent) {
                return Some(parent.clone());
            }
        }
        None
    }

    fn compare(tree_output: String, expected_result: String) -> bool {
        let expected_output = normalize_graph(expected_result);
        let (exp_labels, exp_tree) = parse(expected_output);
        let (out_labels, out_tree) = parse(tree_output);
        let exp_root = find_root(&exp_tree);
        let out_root = find_root(&out_tree);
        println!("{exp_root:#?}, {out_root:#?}");
        let mut exp_not_visited = match exp_root {
            Some(root) => vec![root],
            None => vec![],
        };
        let mut out_not_visited = match out_root {
            Some(root) => vec![root],
            None => vec![],
        };
        loop {
            let curr_exp = exp_not_visited.pop();
            let curr_out = out_not_visited.pop();
            match (curr_exp, curr_out) {
                (None, None) => break,
                (Some(curr_exp), Some(curr_out)) => {
                    let exp_label = exp_labels.get(&curr_exp);
                    let out_label = out_labels.get(&curr_out);
                    match (exp_label, out_label) {
                        (Some(exp_label), Some(out_label)) => {
                            println!(
                                "exp -> {curr_exp}, {exp_label}. out -> {curr_out}, {out_label}"
                            );
                            if exp_label != out_label {
                                return false;
                            }
                            let exp_children = exp_tree.get(&curr_exp);
                            let out_children = out_tree.get(&curr_out);
                            match (exp_children, out_children) {
                                (None, None) => {}
                                (Some(exp_children), Some(out_children)) => {
                                    let mut exp_id_labels = exp_children
                                        .iter()
                                        .map(|id| exp_labels.get_key_value(id))
                                        .filter_map(|id| id)
                                        .collect::<Vec<_>>();

                                    let mut out_id_labels = out_children
                                        .iter()
                                        .map(|id| out_labels.get_key_value(id))
                                        .filter_map(|id| id)
                                        .collect::<Vec<_>>();

                                    exp_id_labels.sort_by_key(|it| it.1);
                                    out_id_labels.sort_by_key(|it| it.1);

                                    exp_id_labels
                                        .into_iter()
                                        .for_each(|exp| exp_not_visited.push(exp.0.clone()));
                                    out_id_labels
                                        .into_iter()
                                        .for_each(|out| out_not_visited.push(out.0.clone()));
                                }
                                _ => {
                                    println!("wtf");
                                    return false;
                                }
                            }
                        }
                        _ => return false,
                    }
                }
                _ => return false,
            }
        }

        true
    }
}
