#[derive(Debug, PartialEq, Eq, Clone)]
pub enum IlocInst {
    Arithm(FullOp),
    LoadImed(InOut),
    LoadDesl(FullOp),
    StoreImed(InOut),
    StoreDesl(In2Out),
    RegCopy(InOut),
    Inv(Inv),
    Cmp(CmpInst),
    Cbr(In2Out),
    Jump(Jump),
    Nop(Option<String>),
    Halt,
    Empty,
}

pub static RETVAL_ADDR: u32 = 0;
pub static RFP_ADDR: u32 = 4;
pub static RSP_ADDR: u32 = 8;
pub static RET_ADDR: u32 = 12;
pub static ADDR_SIZE: u32 = 4;
pub static RESERV_MEM: u32 = ADDR_SIZE * 4;

impl IlocInst {
    pub fn add_arithm_inst(&mut self, name: String) {
        match self {
            IlocInst::Arithm(op) => op.name = name,
            _ => panic!("Should not add name to a non arithm inst"),
        }
    }

    pub fn code_str(&self) -> String {
        match self {
            IlocInst::Arithm(op) => op.code_str(),
            IlocInst::LoadImed(op) => op.code_str(),
            IlocInst::LoadDesl(op) => op.code_str(),
            IlocInst::StoreImed(op) => op.code_str(),
            IlocInst::StoreDesl(op) => op.code_str(),
            IlocInst::RegCopy(op) => op.code_str(),
            IlocInst::Cmp(op) => op.code_str(),
            IlocInst::Cbr(op) => op.code_str(),
            IlocInst::Jump(op) => op.code_str(),
            IlocInst::Inv(op) => op.code_str(),
            IlocInst::Nop(label) => {
                let mut code_str = "".to_string();
                if let Some(label) = label {
                    code_str += &format!("{label}: ");
                }
                code_str += &format!("nop\n");
                code_str
            }
            IlocInst::Halt => format!("halt\n"),
            IlocInst::Empty => "".to_string(),
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
            IlocInst::Inv(mut inst) => {
                inst.add_label(label);
                IlocInst::Inv(inst)
            }
            IlocInst::Nop(_) => IlocInst::Nop(Some(label)),
            IlocInst::Halt => IlocInst::Halt,
            IlocInst::Empty => IlocInst::Empty,
        }
    }

    pub fn get_label(&self) -> Option<String> {
        match self {
            IlocInst::Arithm(inst) => inst.label.clone(),
            IlocInst::LoadImed(inst) => inst.label.clone(),
            IlocInst::LoadDesl(inst) => inst.label.clone(),
            IlocInst::StoreImed(inst) => inst.label.clone(),
            IlocInst::StoreDesl(inst) => inst.label.clone(),
            IlocInst::RegCopy(inst) => inst.label.clone(),
            IlocInst::Inv(inst) => inst.label.clone(),
            IlocInst::Cmp(inst) => inst.label.clone(),
            IlocInst::Cbr(inst) => inst.label.clone(),
            IlocInst::Jump(inst) => inst.label.clone(),
            IlocInst::Nop(label) => label.clone(),
            IlocInst::Halt => None,
            IlocInst::Empty => None,
        }
    }

    pub fn should_fall_through(&self) -> bool {
        match self {
            IlocInst::Cbr(_) => false,
            IlocInst::Jump(_) => false,
            IlocInst::Halt => false,
            IlocInst::Empty => false,
            _ => true,
        }
    }

    pub fn get_jump_labels(&self) -> Vec<String> {
        match self {
            IlocInst::Cbr(inst) => vec![inst.dest.clone(), inst.desl.clone()],
            IlocInst::Jump(inst) => vec![inst.dest.clone()],
            _ => vec![],
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

    pub fn code_str(&self) -> String {
        let mut code_str = "".to_string();
        if let Some(label) = &self.label {
            code_str += &format!("{label}: ");
        }
        code_str += &format!(
            "{} {}, {} => {}\n",
            self.name, self.op1, self.op2, self.dest
        );
        code_str
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

    pub fn code_str(&self) -> String {
        let mut code_str = "".to_string();
        if let Some(label) = &self.label {
            code_str += &format!("{label}: ");
        }
        code_str += &format!(
            "{} {}, {} -> {}\n",
            self.name, self.op1, self.op2, self.dest
        );
        code_str
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

    pub fn code_str(&self) -> String {
        let mut code_str = "".to_string();
        if let Some(label) = &self.label {
            code_str += &format!("{label}: ");
        }
        code_str += &format!("{} {} => {}\n", self.name, self.op, self.dest);
        code_str
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

    pub fn code_str(&self) -> String {
        let mut code_str = "".to_string();
        if let Some(label) = &self.label {
            code_str += &format!("{label}: ");
        }
        code_str += &format!(
            "{} {} => {}, {}\n",
            self.name, self.op, self.dest, self.desl
        );
        code_str
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

    pub fn code_str(&self) -> String {
        let mut code_str = "".to_string();
        if let Some(label) = &self.label {
            code_str += &format!("{label}: ");
        }
        code_str += &format!("{} => {}\n", self.name, self.dest);
        code_str
    }

    pub fn add_label(&mut self, label: String) {
        self.label = Some(label);
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Inv {
    pub orig: String,
    pub dest: String,
    pub label: Option<String>,
}

impl Inv {
    pub fn new(orig: String, dest: String) -> Self {
        Self {
            orig,
            dest,
            label: None,
        }
    }

    pub fn code_str(&self) -> String {
        let mut code_str = "".to_string();
        if let Some(label) = &self.label {
            code_str += &format!("{label}:\n");
        }
        code_str += &format!("multI {}, -1 => {}\n", self.orig, self.dest);
        code_str
    }

    pub fn add_label(&mut self, label: String) {
        self.label = Some(label);
    }
}

pub fn save_rfp_rsp() -> Vec<IlocInst> {
    let save_rfp = IlocInst::StoreDesl(In2Out::new(
        "storeAI".to_string(),
        "rfp".to_string(),
        "rsp".to_string(),
        RFP_ADDR.to_string(),
    ));

    let save_rsp = IlocInst::StoreDesl(In2Out::new(
        "storeAI".to_string(),
        "rsp".to_string(),
        "rsp".to_string(),
        RSP_ADDR.to_string(),
    ));

    vec![save_rfp, save_rsp]
}
