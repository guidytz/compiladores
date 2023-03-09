#[derive(Debug, PartialEq, Eq, Clone)]
pub enum IlocInst {
    Arithm(FullOp),
    LoadImed(InOut),
    LoadDesl(FullOp),
    StoreImed(InOut),
    StoreDesl(In2Out),
    RegCopy(InOut),
    Cmp(FullOp),
    Cbr(In2Out),
    Jump(Jump),
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
            IlocInst::Empty => (),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FullOp {
    pub name: String,
    pub op1: String,
    pub op2: String,
    pub dest: String,
}

impl FullOp {
    pub fn new(name: String, op1: String, op2: String, dest: String) -> Self {
        Self {
            name,
            op1,
            op2,
            dest,
        }
    }

    pub fn print(&self) {
        println!("{} {}, {} => {}", self.name, self.op1, self.op2, self.dest);
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct InOut {
    pub name: String,
    pub op: String,
    pub dest: String,
}

impl InOut {
    pub fn new(name: String, op: String, dest: String) -> Self {
        Self { name, op, dest }
    }

    pub fn print(&self) {
        println!("{} {} => {}", self.name, self.op, self.dest);
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct In2Out {
    pub name: String,
    pub op: String,
    pub dest: String,
    pub desl: String,
}

impl In2Out {
    pub fn new(name: String, op: String, dest: String, desl: String) -> Self {
        Self {
            name,
            op,
            dest,
            desl,
        }
    }

    pub fn print(&self) {
        println!("{} {} => {}, {}", self.name, self.op, self.dest, self.desl);
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Jump {
    pub name: String,
    pub dest: String,
}

impl Jump {
    pub fn new(name: String, dest: String) -> Self {
        Self { name, dest }
    }

    pub fn print(&self) {
        println!("{} => {}", self.name, self.dest);
    }
}
