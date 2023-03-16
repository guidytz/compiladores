#[derive(Debug, PartialEq, Eq, Clone)]
pub enum IlocInst {
    Arithm(FullOp),
    LoadImed(InOut),
    LoadDesl(FullOp),
    StoreImed(InOut),
    StoreDesl(In2Out),
    RegCopy(InOut),
    Cmp(CmpInst),
    Cbr(In2Out),
    Jump(Jump),
    Nop(Option<String>),
    Halt,
    Empty,
}

impl IlocInst {
    pub fn add_arithm_inst(&mut self, name: String) {
        match self {
            IlocInst::Arithm(op) => op.name = name,
            _ => panic!("Should not add name to a non arithm inst"),
        }
    }

    pub fn print(&self) {
        match self {
            IlocInst::Arithm(op) => op.print(),
            IlocInst::LoadImed(op) => op.print(),
            IlocInst::LoadDesl(op) => op.print(),
            IlocInst::StoreImed(op) => op.print(),
            IlocInst::StoreDesl(op) => op.print(),
            IlocInst::RegCopy(op) => op.print(),
            IlocInst::Cmp(op) => op.print(),
            IlocInst::Cbr(op) => op.print(),
            IlocInst::Jump(op) => op.print(),
            IlocInst::Nop(label) => {
                if let Some(label) = label {
                    print!("{label}: ");
                }
                println!("nop");
            }
            IlocInst::Halt => println!("halt"),
            IlocInst::Empty => (),
        }
    }

    pub fn add_label(self, label: String) -> Self {
        match self {
            IlocInst::Arithm(mut inst) => {
                inst.add_label(label);
                IlocInst::Arithm(inst)
            }
            IlocInst::LoadImed(mut inst) => {
                inst.add_label(label);
                IlocInst::LoadImed(inst)
            }
            IlocInst::LoadDesl(mut inst) => {
                inst.add_label(label);
                IlocInst::LoadDesl(inst)
            }
            IlocInst::StoreImed(mut inst) => {
                inst.add_label(label);
                IlocInst::StoreImed(inst)
            }
            IlocInst::StoreDesl(mut inst) => {
                inst.add_label(label);
                IlocInst::StoreDesl(inst)
            }
            IlocInst::RegCopy(mut inst) => {
                inst.add_label(label);
                IlocInst::RegCopy(inst)
            }
            IlocInst::Cmp(mut inst) => {
                inst.add_label(label);
                IlocInst::Cmp(inst)
            }
            IlocInst::Cbr(mut inst) => {
                inst.add_label(label);
                IlocInst::Cbr(inst)
            }
            IlocInst::Jump(mut inst) => {
                inst.add_label(label);
                IlocInst::Jump(inst)
            }
            IlocInst::Nop(_) => IlocInst::Nop(Some(label)),
            IlocInst::Halt => IlocInst::Halt,
            IlocInst::Empty => IlocInst::Empty,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FullOp {
    pub name: String,
    pub op1: String,
    pub op2: String,
    pub dest: String,
    pub label: Option<String>,
}

impl FullOp {
    pub fn new(name: String, op1: String, op2: String, dest: String) -> Self {
        Self {
            name,
            op1,
            op2,
            dest,
            label: None,
        }
    }

    pub fn print(&self) {
        if let Some(label) = &self.label {
            print!("{label}: ");
        }
        println!("{} {}, {} => {}", self.name, self.op1, self.op2, self.dest);
    }

    pub fn add_label(&mut self, label: String) {
        self.label = Some(label);
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct CmpInst {
    pub name: String,
    pub op1: String,
    pub op2: String,
    pub dest: String,
    pub label: Option<String>,
}

impl CmpInst {
    pub fn new(name: String, op1: String, op2: String, dest: String) -> Self {
        Self {
            name,
            op1,
            op2,
            dest,
            label: None,
        }
    }

    pub fn print(&self) {
        if let Some(label) = &self.label {
            print!("{label}: ");
        }
        println!("{} {}, {} -> {}", self.name, self.op1, self.op2, self.dest);
    }

    pub fn add_label(&mut self, label: String) {
        self.label = Some(label);
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct InOut {
    pub name: String,
    pub op: String,
    pub dest: String,
    pub label: Option<String>,
}

impl InOut {
    pub fn new(name: String, op: String, dest: String) -> Self {
        Self {
            name,
            op,
            dest,
            label: None,
        }
    }

    pub fn print(&self) {
        if let Some(label) = &self.label {
            print!("{label}: ");
        }
        println!("{} {} => {}", self.name, self.op, self.dest);
    }

    pub fn add_label(&mut self, label: String) {
        self.label = Some(label);
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct In2Out {
    pub name: String,
    pub op: String,
    pub dest: String,
    pub desl: String,
    pub label: Option<String>,
}

impl In2Out {
    pub fn new(name: String, op: String, dest: String, desl: String) -> Self {
        Self {
            name,
            op,
            dest,
            desl,
            label: None,
        }
    }

    pub fn print(&self) {
        if let Some(label) = &self.label {
            print!("{label}: ");
        }
        println!("{} {} => {}, {}", self.name, self.op, self.dest, self.desl);
    }

    pub fn add_label(&mut self, label: String) {
        self.label = Some(label);
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Jump {
    pub name: String,
    pub dest: String,
    pub label: Option<String>,
}

impl Jump {
    pub fn new(name: String, dest: String) -> Self {
        Self {
            name,
            dest,
            label: None,
        }
    }

    pub fn print(&self) {
        if let Some(label) = &self.label {
            print!("{label}: ");
        }
        println!("{} => {}", self.name, self.dest);
    }

    pub fn add_label(&mut self, label: String) {
        self.label = Some(label);
    }
}
