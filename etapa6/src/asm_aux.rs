use crate::semantic_aux::SymbolEntry;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum AsmInst {
    Arithm(FullOp),
    Not(Not),
    MovImed(InOut),
    Mov(Mov),
    LoadDesl(LoadDesl),
    StoreImed(InOut),
    StoreDesl(In2Out),
    CmpReg(CmpReg),
    Cmp(CmpInst),
    Jump(Jump),
    StackInst(StackInst),
    Nop(Option<String>),
    SingleInst(Option<String>, String),
    Directive(Directive),
    Halt,
    Empty,
}

pub static RETURN_AND_RBP_OFFSET: u32 = 16;

impl AsmInst {
    pub fn add_arithm_inst(&mut self, name: String) {
        match self {
            AsmInst::Arithm(op) => op.name = name,
            _ => panic!("Should not add name to a non arithm inst"),
        }
    }

    pub fn code_str(&self) -> String {
        match self {
            AsmInst::Arithm(op) => op.code_str(),
            AsmInst::MovImed(op) => op.code_str(),
            AsmInst::LoadDesl(op) => op.code_str(),
            AsmInst::StoreImed(op) => op.code_str(),
            AsmInst::StoreDesl(op) => op.code_str(),
            AsmInst::CmpReg(op) => op.code_str(),
            AsmInst::Cmp(op) => op.code_str(),
            AsmInst::Jump(op) => op.code_str(),
            AsmInst::Nop(label) => {
                let mut code_str = "".to_string();
                if let Some(label) = label {
                    code_str += &format!("{label}: ");
                }
                code_str += &format!("\tnop\n");
                code_str
            }
            AsmInst::Halt => format!("\thlt\n"),
            AsmInst::Empty => "".to_string(),
            AsmInst::StackInst(inst) => inst.code_str(),
            AsmInst::SingleInst(label, name) => {
                let mut code_str = "".to_string();
                if let Some(label) = label {
                    code_str += &format!("{label}: ");
                }
                code_str += &format!("\t{name}\n");
                code_str
            }
            AsmInst::Directive(dir) => dir.code_str(),
            AsmInst::Mov(inst) => inst.code_str(),
            AsmInst::Not(inst) => inst.code_str(),
        }
    }

    pub fn add_label(self, label: String) -> Self {
        match self {
            AsmInst::Arithm(mut inst) => {
                inst.add_label(label);
                AsmInst::Arithm(inst)
            }
            AsmInst::MovImed(mut inst) => {
                inst.add_label(label);
                AsmInst::MovImed(inst)
            }
            AsmInst::LoadDesl(mut inst) => {
                inst.add_label(label);
                AsmInst::LoadDesl(inst)
            }
            AsmInst::StoreImed(mut inst) => {
                inst.add_label(label);
                AsmInst::StoreImed(inst)
            }
            AsmInst::StoreDesl(mut inst) => {
                inst.add_label(label);
                AsmInst::StoreDesl(inst)
            }
            AsmInst::CmpReg(mut inst) => {
                inst.add_label(label);
                AsmInst::CmpReg(inst)
            }
            AsmInst::Cmp(mut inst) => {
                inst.add_label(label);
                AsmInst::Cmp(inst)
            }
            AsmInst::Jump(mut inst) => {
                inst.add_label(label);
                AsmInst::Jump(inst)
            }
            AsmInst::Nop(_) => AsmInst::Nop(Some(label)),
            AsmInst::Halt => AsmInst::Halt,
            AsmInst::Empty => AsmInst::Empty,
            AsmInst::StackInst(mut inst) => {
                inst.add_label(label);
                AsmInst::StackInst(inst)
            }
            AsmInst::SingleInst(_, name) => AsmInst::SingleInst(Some(label), name),
            AsmInst::Directive(mut dir) => {
                dir.add_label(label);
                AsmInst::Directive(dir)
            }
            AsmInst::Mov(mut inst) => {
                inst.add_label(label);
                AsmInst::Mov(inst)
            }
            AsmInst::Not(mut inst) => {
                inst.add_label(label);
                AsmInst::Not(inst)
            }
        }
    }

    pub fn patch_jump<'a>(self, name: &'a str) -> Self {
        match self {
            AsmInst::Jump(mut jump) => {
                let dest = jump.dest.replace("patch", name);
                jump.dest = dest;
                AsmInst::Jump(jump)
            }
            _ => self,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FullOp {
    pub name: String,
    pub op1: String,
    pub op2: String,
    pub label: Option<String>,
}

impl FullOp {
    pub fn new(name: String, op1: String, op2: String) -> Self {
        Self {
            name,
            op1,
            op2,
            label: None,
        }
    }

    pub fn code_str(&self) -> String {
        let mut code_str = "".to_string();
        if let Some(label) = &self.label {
            code_str += &format!("{label}: ");
        }
        code_str += &format!("\t{}\t{}, {}\n", self.name, self.op1, self.op2);
        code_str
    }

    pub fn add_label(&mut self, label: String) {
        self.label = Some(label);
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Directive {
    pub name: String,
    pub op1: Option<String>,
    pub op2: Option<String>,
    pub label: Option<String>,
}

impl Directive {
    pub fn new(name: String, op1: Option<String>, op2: Option<String>) -> Self {
        Self {
            name,
            op1,
            op2,
            label: None,
        }
    }

    pub fn code_str(&self) -> String {
        let mut code_str = "".to_string();
        if let Some(label) = &self.label {
            code_str += &format!("{label}: ");
        }
        code_str += &format!("\t.{}", self.name);

        if let Some(op1) = &self.op1 {
            code_str += &format!("\t{}", op1);
        }

        if let Some(op2) = &self.op2 {
            code_str += &format!(", {}", op2);
        }
        code_str += &format!("\n");
        code_str
    }

    pub fn add_label(&mut self, label: String) {
        self.label = Some(label);
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Mov {
    pub name: String,
    pub source: String,
    pub dest: String,
    pub label: Option<String>,
}

impl Mov {
    pub fn new(name: String, source: String, dest: String) -> Self {
        Self {
            name,
            source,
            dest,
            label: None,
        }
    }

    pub fn code_str(&self) -> String {
        let mut code_str = "".to_string();
        if let Some(label) = &self.label {
            code_str += &format!("{label}: ");
        }
        code_str += &format!("\t{}\t{}, {}\n", self.name, self.source, self.dest);
        code_str
    }

    pub fn add_label(&mut self, label: String) {
        self.label = Some(label);
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct CmpReg {
    pub reg1: String,
    pub reg2: String,
    pub label: Option<String>,
}

impl CmpReg {
    pub fn new(reg1: String, reg2: String) -> Self {
        Self {
            reg1,
            reg2,
            label: None,
        }
    }

    pub fn code_str(&self) -> String {
        let mut code_str = "".to_string();
        if let Some(label) = &self.label {
            code_str += &format!("{label}: ");
        }
        code_str += &format!("\tcmp\t\t{}, {}\n", self.reg1, self.reg2);
        code_str
    }

    pub fn add_label(&mut self, label: String) {
        self.label = Some(label);
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct LoadDesl {
    pub name: String,
    pub desl: String,
    pub reg1: String,
    pub reg2: String,
    pub label: Option<String>,
}

impl LoadDesl {
    pub fn new(name: String, desl: String, reg1: String, reg2: String) -> Self {
        Self {
            name,
            desl,
            reg1,
            reg2,
            label: None,
        }
    }

    pub fn code_str(&self) -> String {
        let mut code_str = "".to_string();
        if let Some(label) = &self.label {
            code_str += &format!("{label}: ");
        }
        code_str += &format!(
            "\t{}\t{}({}), {}\n",
            self.name, self.desl, self.reg1, self.reg2
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
    pub dest: String,
    pub label: Option<String>,
}

impl CmpInst {
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
        code_str += &format!("\t{}\t\t{}\n", self.name, self.dest);
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
        code_str += &format!("\t{}\t{}, {}\n", self.name, self.op, self.dest);
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
            "\t{}\t{}, {}({})\n",
            self.name, self.op, self.desl, self.dest
        );
        code_str
    }

    pub fn add_label(&mut self, label: String) {
        self.label = Some(label);
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Jump {
    pub dest: String,
    pub label: Option<String>,
}

impl Jump {
    pub fn new(dest: String) -> Self {
        Self { dest, label: None }
    }

    pub fn code_str(&self) -> String {
        let mut code_str = "".to_string();
        if let Some(label) = &self.label {
            code_str += &format!("{label}: ");
        }
        code_str += &format!("\tjmp\t\t{}\n", self.dest);
        code_str
    }

    pub fn add_label(&mut self, label: String) {
        self.label = Some(label);
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Not {
    pub dest: String,
    pub label: Option<String>,
}

impl Not {
    pub fn new(dest: String) -> Self {
        Self { dest, label: None }
    }

    pub fn code_str(&self) -> String {
        let mut code_str = "".to_string();
        if let Some(label) = &self.label {
            code_str += &format!("{label}: ");
        }
        code_str += &format!("\tnot\t\t{}\n", self.dest);
        code_str
    }

    pub fn add_label(&mut self, label: String) {
        self.label = Some(label);
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct StackInst {
    pub name: String,
    pub dest: String,
    pub label: Option<String>,
}

impl StackInst {
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
        code_str += &format!("\t{}\t{}\n", self.name, self.dest);
        code_str
    }

    pub fn add_label(&mut self, label: String) {
        self.label = Some(label);
    }
}

pub fn gen_global_declr_code(symbol: &SymbolEntry) -> Vec<AsmInst> {
    let mut code = vec![];

    let text_directive = AsmInst::Directive(Directive::new("text".to_string(), None, None));
    let globl_directive = AsmInst::Directive(Directive::new(
        "globl".to_string(),
        Some(symbol.name()),
        None,
    ));
    let bss_directive = AsmInst::Directive(Directive::new("bss".to_string(), None, None));
    let align_directive = AsmInst::Directive(Directive::new(
        "align".to_string(),
        Some(symbol.size().to_string()),
        None,
    ));
    let type_directive = AsmInst::Directive(Directive::new(
        "type".to_string(),
        Some(symbol.name()),
        Some("@object".to_string()),
    ));
    let size_directive = AsmInst::Directive(Directive::new(
        "size".to_string(),
        Some(symbol.name()),
        Some(symbol.size().to_string()),
    ));
    let zero_directive = AsmInst::Directive(Directive::new(
        "zero".to_string(),
        Some(symbol.size().to_string()),
        None,
    ))
    .add_label(symbol.name());

    code.push(text_directive);
    code.push(globl_directive);
    code.push(bss_directive);
    code.push(align_directive);
    code.push(type_directive);
    code.push(size_directive);
    code.push(zero_directive);

    code
}

pub fn patch_returns<'a>(name: &'a str, code: Vec<AsmInst>) -> Vec<AsmInst> {
    code.into_iter()
        .map(|inst| inst.patch_jump(&name))
        .collect()
}
