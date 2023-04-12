use crate::iloc_aux::IlocInst;

#[derive(Debug, Clone)]
pub struct FluxCtrlGraphNode {
    pub id: String,
    pub code: Vec<IlocInst>,
    pub children: Vec<Box<FluxCtrlGraphNode>>,
}

impl FluxCtrlGraphNode {
    pub fn new(id: u32, code: Vec<IlocInst>) -> Self {
        let id = format!("node_{id}");
        Self {
            id,
            code,
            children: vec![],
        }
    }

    pub fn add_child(&mut self, child: FluxCtrlGraphNode) {
        self.children.push(Box::new(child))
    }

    pub fn is_destiny_node<'a>(&self, label: &'a str) -> bool {
        if let Some(first) = self.code.first() {
            if label.contains("r") {
                match first {
                    IlocInst::Halt => return true,
                    _ => return false,
                }
            }
            if let Some(inst_label) = first.get_label() {
                return label == inst_label;
            }
        }
        return false;
    }
}

pub fn print_graph(graph: Vec<FluxCtrlGraphNode>) {
    println!("digraph {{");
    for node in graph {
        let code_str = node
            .code
            .iter()
            .map(|inst| inst.code_str())
            .collect::<String>();

        println!("{} [label=\"{}\"];", node.id, code_str);
        node.children
            .iter()
            .for_each(|child| println!("{} -> {}", node.id, child.id));
    }
    println!("}}");
}

pub fn build_graph(code: Vec<IlocInst>) -> Vec<FluxCtrlGraphNode> {
    let leaders = find_leaders(&code);
    let b_blocks = get_basic_blocks(&leaders, &code);
    let mut id = 0;
    let nodes = b_blocks
        .into_iter()
        .map(|(start, end)| {
            let node_code = code
                .iter()
                .enumerate()
                .filter(|(idx, _)| *idx >= start && *idx <= end)
                .map(|(_, inst)| inst)
                .cloned()
                .collect::<Vec<_>>();
            id += 1;
            FluxCtrlGraphNode::new(id, node_code)
        })
        .collect::<Vec<_>>();

    let mut graph = nodes
        .clone()
        .into_iter()
        .map(|mut node| {
            let mut jump_labels = vec![];
            node.code
                .iter()
                .for_each(|inst| jump_labels.extend(inst.get_jump_labels()));
            for dest_node in nodes.clone() {
                jump_labels.iter().for_each(|label| {
                    if dest_node.is_destiny_node(&label) {
                        node.add_child(dest_node.clone());
                    }
                })
            }

            node
        })
        .collect::<Vec<_>>();

    let mut curr = 0;
    let mut next = 1;
    while next <= graph.len() {
        if let Some(last) = graph[curr].code.last() {
            if last.should_fall_through() {
                let dest_node = graph[next].clone();
                graph[curr].add_child(dest_node);
            }
        }
        curr += 1;
        next += 1;
    }
    // graph.windows(2).map(|&mut window| {
    // });
    graph
}

#[cfg(feature = "code")]
fn find_leaders(code: &[IlocInst]) -> Vec<usize> {
    let mut leader_set = std::collections::HashSet::new();
    leader_set.extend([0, 7]); // adding first inst and return from main

    code.iter().enumerate().for_each(|(idx, inst)| {
        if let Some(_) = inst.get_label() {
            leader_set.insert(idx);
        }
        match inst {
            IlocInst::Cbr(_) if idx < code.len() - 1 => {
                leader_set.insert(idx + 1);
            }
            IlocInst::Jump(_) if idx < code.len() - 1 => {
                leader_set.insert(idx + 1);
            }
            _ => (),
        }
    });

    let mut leader_set = leader_set.drain().collect::<Vec<_>>();
    leader_set.sort();
    leader_set
}

fn get_basic_blocks(leaders: &[usize], code: &[IlocInst]) -> Vec<(usize, usize)> {
    let mut blocks = leaders
        .windows(2)
        .map(|window| (window[0], window[1] - 1))
        .collect::<Vec<_>>();
    blocks.push((leaders.last().unwrap().clone(), code.len() - 1));
    blocks
}
