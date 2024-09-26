mod opcode;

use clap::Parser;
use std::fs;


// Global debug mode flag
static DEBUG_MODE: bool = true;

macro_rules! debug_println {
    ($($arg:tt)*) => {
        if DEBUG_MODE {
            println!($($arg)*);
        }
    };
}

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Cli {
    #[arg(short, long)]
    dump: bool
}

enum OpType {
    R,   // Register
    IMM, // Immediate (byte)
    IMM2, // Immediate (word)
    IMM4, // Immediate (dword)
}

const INSTRUCTIONS: &[(&str, &[OpType])] = &[
    ("halt", &[]),
    ("add", &[OpType::R, OpType::R, OpType::R]),
    ("sub", &[OpType::R, OpType::R, OpType::R]),
    ("lw", &[OpType::R, OpType::IMM]),
    ("sw", &[OpType::R, OpType::IMM]),
    ("beq", &[OpType::R, OpType::R, OpType::IMM]),
    ("addib", &[OpType::R, OpType::R, OpType::IMM]),
    ("addiw", &[OpType::R, OpType::R, OpType::IMM2]),
    ("addid", &[OpType::R, OpType::R, OpType::IMM4]),
    ("bneq", &[OpType::R, OpType::R, OpType::IMM])
];

fn register_to_bin(reg: &str) -> Result<u8, String> {
    if reg.starts_with('$') {
        reg[1..]
            .parse::<u8>()
            .map_err(|_| "Invalid register".to_string())
    } else {
        Err("Invalid register format".to_string())
    }
}

fn assemble_instruction(instruction: &str) -> Result<Vec<u8>, String> {
    let clean_instruction = instruction.replace(',', "").trim().to_string();
    let parts: Vec<&str> = clean_instruction.split_whitespace().collect();

    if parts.is_empty() {
        return Err("Empty instruction".to_string());
    }

    let (opcode_index, expected_types) = INSTRUCTIONS
        .iter()
        .enumerate()
        .find(|(_, (name, _))| *name == parts[0])
        .map(|(index, (_, types))| (index as u8, types))
        .ok_or("Unknown opcode".to_string())?;

    if parts.len() - 1 != expected_types.len() {
        return Err(format!(
            "Incorrect number of operands for {}: expected {}, found {}",
            parts[0],
            expected_types.len(),
            parts.len() - 1
        ));
    }

    let mut assembled_instruction = vec![opcode_index];

    for (i, op_type) in expected_types.iter().enumerate() {
        match op_type {
            OpType::R => {
                let reg = register_to_bin(parts[i + 1])?;
                assembled_instruction.push(reg);
            }
            OpType::IMM => {
                let imm: i8 = parts[i + 1]
                    .parse()
                    .map_err(|_| "Invalid immediate value".to_string())?;
                assembled_instruction.push(imm as u8);
            }
            OpType::IMM2 => {
                let imm: i16 = parts[i+1]
                .parse()
                .map_err(|_| "Invalid imm2".to_string())?;
                assembled_instruction.extend(imm.to_le_bytes().iter());
            }
            OpType::IMM4 => {
                let imm: i32 = parts[i+1]
                .parse()
                .map_err(|_| "Invalid imm4".to_string())?;
                assembled_instruction.extend(imm.to_le_bytes().iter());
            }
        }
    }

    Ok(assembled_instruction)
}

/// Remove comments from the program (single-line and multi-line)
fn remove_comments(program: &str) -> String {
    let mut in_multiline_comment = false;
    let mut clean_code = String::new();

    for line in program.lines() {
        let mut trimmed_line = line.trim().to_string();

        // Handle multi-line comments (/* ... */)
        if in_multiline_comment {
            if let Some(end) = trimmed_line.find("*/") {
                trimmed_line = trimmed_line[(end + 2)..].to_string();
                in_multiline_comment = false;
            } else {
                continue; // Skip the entire line if we're still in a comment
            }
        }

        // Remove anything after `/*`
        if let Some(start) = trimmed_line.find("/*") {
            if let Some(end) = trimmed_line.find("*/") {
                // If `*/` is in the same line, remove the comment
                trimmed_line.replace_range(start..=end + 1, "");
            } else {
                // Otherwise, mark we're inside a comment and remove everything after `/*`
                trimmed_line.replace_range(start.., "");
                in_multiline_comment = true;
            }
        }

        // Remove anything after `;` for single-line comments
        if let Some(comment_pos) = trimmed_line.find(';') {
            trimmed_line = trimmed_line[..comment_pos].to_string();
        }

        // Add the cleaned line if it's not empty
        if !trimmed_line.trim().is_empty() {
            clean_code.push_str(&trimmed_line);
            clean_code.push('\n');
        }
    }

    clean_code
}

fn assemble_program(program: &str) -> Result<Vec<u8>, String> {
    puffin::profile_scope!("vm::assemble");
    let mut bytecode = Vec::new();

    let cleaned_program = remove_comments(program);

    for line in cleaned_program.lines() {
        let line = line.trim();
        if !line.is_empty() {
            let instruction = assemble_instruction(line)?;
            bytecode.extend(instruction);
        }
    }
    Ok(bytecode)
}

pub struct VM {
    registers: [u32; 32],
    memory: [u8; 1024],
    pc: usize,
}

impl Default for VM {
    fn default() -> Self {
        Self {
            registers: Default::default(),
            memory: [0; 1024],
            pc: Default::default(),
        }
    }
}
use opcode::OPCODE;
impl VM {
    fn new() -> Self {
        VM {
            registers: [0; 32],
            memory: [0; 1024],
            pc: 0,
        }
    }

    // some sorta binary stream reader

    fn fetch_byte(&mut self, program: &[u8]) -> u8 {
        let byte = program[self.pc];
        self.pc += 1;
        byte
    }

    fn fetch_word(&mut self, program: &[u8]) -> u16 {
        let low_byte = self.fetch_byte(program) as u16;
        let high_byte = self.fetch_byte(program) as u16;
        low_byte | (high_byte << 8)
    }

    fn fetch_dword(&mut self, program: &[u8]) -> u32 {
        let b1 = self.fetch_byte(program) as u32;
        let b2 = self.fetch_byte(program) as u32;
        let b3 = self.fetch_byte(program) as u32;
        let b4 = self.fetch_byte(program) as u32;
        b1 | (b2 << 8) | (b3 << 16) | (b4 << 24)
    }

    fn fetch_bytes(&mut self, program: &[u8], n: usize) -> Vec<u8> {
        let mut bytes = Vec::with_capacity(n);
        for _ in 0..n {
            bytes.push(self.fetch_byte(program));
        }
        bytes
    }

    fn execute(&mut self, program: &[u8]) {
        puffin::profile_scope!("vm::execute");
        self.pc = 0; // reset pc (aka fix run once glithc)
        while self.pc < program.len() {
            let opcode = self.fetch_byte(program);

            let op = OPCODE::from_repr(opcode);

            match op.unwrap_or(OPCODE::HALT) {
                OPCODE::HALT => {
                    // HALT
                    debug_println!("HALT, stopping execution...");
                    return;
                }
                OPCODE::ADD => {
                    let rd = self.fetch_byte(program) as usize;
                    let rs = self.fetch_byte(program) as usize;
                    let rt = self.fetch_byte(program) as usize;
                    self.registers[rd] = self.registers[rs] + self.registers[rt];
                    debug_println!("ADD executed: R{} = R{} + R{}", rd, rs, rt);
                }
                OPCODE::SUB => {
                    let rd = self.fetch_byte(program) as usize;
                    let rs = self.fetch_byte(program) as usize;
                    let rt = self.fetch_byte(program) as usize;
                    self.registers[rd] = self.registers[rs] - self.registers[rt];
                    debug_println!("SUB executed: R{} = R{} - R{}", rd, rs, rt);
                }
                OPCODE::LW => {
                    let rd = self.fetch_byte(program) as usize;
                    let offset = self.fetch_byte(program) as usize;
                    self.registers[rd] = self.memory[offset] as u32;
                    debug_println!("LW executed: R{} = M[{}]", rd, offset);
                }
                OPCODE::SW => {
                    let rs = self.fetch_byte(program) as usize;
                    let offset = self.fetch_byte(program) as usize;
                    self.memory[offset] = self.registers[rs] as u8;
                    debug_println!("SW executed: M[{}] = R{}", offset, rs);
                }
                OPCODE::BEQ => {
                    let rs = self.fetch_byte(program) as usize;
                    let rt = self.fetch_byte(program) as usize;
                    let offset = self.fetch_byte(program) as usize;
                    if self.registers[rs] == self.registers[rt] {
                        self.pc = offset;
                        debug_println!("BEQ executed: R{} == R{}, jumping to {}", rs, rt, offset);
                    }
                }
                OPCODE::ADDIB => {
                    let rs = self.fetch_byte(program) as usize;
                    let rt = self.fetch_byte(program) as usize;
                    let imm = self.fetch_byte(program) as u8;
                    self.registers[rs] = self.registers[rt] + imm as u32;
                    debug_println!("ADDIB executed: R{} = R{} + {}", rs, rt, imm);
                }
                OPCODE::ADDIW => {
                    let rs = self.fetch_byte(program) as usize;
                    let rt = self.fetch_byte(program) as usize;
                    let imm = self.fetch_word(program) as u16;
                    self.registers[rs] = self.registers[rt] + imm as u32;
                    debug_println!("ADDIW executed: R{} = R{} + {}", rs, rt, imm);

                }
                OPCODE::ADDID => {
                    let rs = self.fetch_byte(program) as usize;
                    let rt = self.fetch_byte(program) as usize;
                    let imm = self.fetch_dword(program) as u32;
                    self.registers[rs] = self.registers[rt] + imm;
                    debug_println!("ADDID executed: R{} = R{} + {}", rs, rt, imm);

                }
                OPCODE::BNEQ => {
                    let rs = self.fetch_byte(program) as usize;
                    let rt = self.fetch_byte(program) as usize;
                    let offset = self.fetch_byte(program) as usize;
                    if self.registers[rs] != self.registers[rt] {
                        self.pc = offset;
                        debug_println!("BNEQ executed: R{} == R{}, jumping to {}", rs, rt, offset);
                    }

                }
                _ => panic!("Unknown opcode {}", opcode),
            }
        }
    }
}

fn hexdump(bytecode: &[u8], start_offset: usize, size: usize, bytes_per_line: usize) {
    let end_offset = (start_offset + size).min(bytecode.len());

    for i in (start_offset..end_offset).step_by(bytes_per_line) {
        print!("{:08x}: ", i);

        for j in i..(i + bytes_per_line).min(end_offset) {
            print!("{:02x} ", bytecode[j]);
        }

        let line_size = (i + bytes_per_line).min(end_offset) - i;
        for _ in line_size..bytes_per_line {
            print!("   ");
        }

        print!(" |");
        for j in i..(i + bytes_per_line).min(end_offset) {
            let byte = bytecode[j];
            if byte.is_ascii_graphic() || byte == b' ' {
                print!("{}", byte as char);
            } else {
                print!(".");
            }
        }
        debug_println!("|");
    }
}

use eframe::egui;
use egui::{FontId, TextEdit};

struct MipsIDE {
    assembly_code: String,
    bytecode: Vec<u8>,
    vm: VM,
    reset_vm: bool,
    is_running: bool,
    step_mode: bool,
}

impl Default for MipsIDE {
    fn default() -> Self {
        Self {
            assembly_code: String::new(),
            bytecode: Vec::new(),
            vm: VM::new(),
            is_running: false,
            step_mode: false,
            reset_vm: false,
        }
    }
}

impl MipsIDE {
    fn new(_cc: &eframe::CreationContext<'_>) -> Self {
        Self::default()
    }
}

impl eframe::App for MipsIDE {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        puffin::GlobalProfiler::lock().new_frame();
        puffin::profile_scope!("update");
        egui::CentralPanel::default().show(ctx, |ui| {
            let mut profile = puffin::are_scopes_on();
            ui.checkbox(&mut profile, "Show profiler window");
            puffin::set_scopes_on(profile); // controls both the profile capturing, and the displaying of it

           

            if ui.button("Quit").clicked() {
                ui.ctx().send_viewport_cmd(egui::ViewportCommand::Close);
            }
        });

        // This call does nothing if profiling is disabled
        puffin_egui::show_viewport_if_enabled(ctx);

        egui::CentralPanel::default().show(ctx, |ui| {
            ui.heading("Assembly Editor and VM Debugger");

            ui.horizontal(|ui| {
                ui.vertical(|ui| {
                    ui.label("Write your assembly code:");
                    ui.add(
                        TextEdit::multiline(&mut self.assembly_code)
                            .font(FontId::new(16.0, egui::FontFamily::Monospace))
                            .desired_rows(20)
                            .desired_width(500.0),
                    );
                });

                ui.vertical(|ui| {
                    ui.horizontal(|ui| {
                        if ui.button("Assemble").clicked() {
                            self.bytecode =
                                assemble_program(&self.assembly_code).unwrap_or_else(|err| {
                                    eprintln!("Assembly error: {}", err);
                                    Vec::new()
                                });
                        }

                        if ui.button("Run").clicked() {
                            if self.reset_vm{
                                self.vm = VM::new();
                            }
                            self.vm.execute(&self.bytecode);
                        }

                        if ui.button("Step").clicked() {
                        }

                        ui.checkbox(&mut self.reset_vm, "Reset VM ?");
                    });
                });
            });

            ui.separator();

            ui.push_id(1967777, |ui| {
                egui::ScrollArea::vertical().show(ui, |ui| {
                    ui.label("Bytecode Hexdump:");
                    hexdump_ui(ui, &self.bytecode, 16);
                });
            });

            ui.separator();

            ui.push_id(98237237, |ui| {
                ui.label("VM State:");

                egui::ScrollArea::vertical().show(ui, |ui| {
                    display_vm_state(ui, &self.vm);
                });
            });
        });
    }
}

fn display_vm_state(ui: &mut egui::Ui, vm: &VM) {
    ui.label("Registers:");
    for (i, reg) in vm.registers.iter().enumerate() {
        ui.label(format!("R{}: 0x{:08x}", i, reg));
    }

    ui.separator();
    ui.label("Memory:");
    for (i, chunk) in vm.memory.chunks(16).enumerate() {
        let addr = i * 16;
        let hex_vals: String = chunk.iter().map(|b| format!("{:02x} ", b)).collect();
        ui.label(format!("0x{:04x}: {}", addr, hex_vals));
    }
}

fn hexdump_ui(ui: &mut egui::Ui, bytecode: &[u8], bytes_per_line: usize) {
    for (i, chunk) in bytecode.chunks(bytes_per_line).enumerate() {
        let offset = i * bytes_per_line;
        let hex_vals: String = chunk.iter().map(|b| format!("{:02x} ", b)).collect();
        ui.label(format!("0x{:04x}: {}", offset, hex_vals));
    }
}


use std::format;



fn main() -> eframe::Result {
    let cli = Cli::parse();
    if cli.dump
    {
        let mut dmp :String = String::new();

        dmp.push_str("use strum::FromRepr;\n/*AUTOGEN*/\n#[derive(FromRepr, Debug, PartialEq)]\n#[repr(u8)]\n");
        dmp.push_str("pub enum OPCODE {\n");
        debug_println!("enum OPCODE\n {{");
        let mut counter = 0;
        for i in INSTRUCTIONS
        {
            debug_println!("\t{}", i.0);
            dmp.push_str(format!("\t{}={},\n", i.0.to_uppercase(), counter).as_str());
            counter+=1;
        }
        dmp.push_str("}");
        match fs::write("src/opcode.rs", dmp) {
            Ok(_) => {debug_println!("File written....")},
            Err(_) => {debug_println!("Exception occured, file not written")},
        }
        debug_println!("}}");
        std::process::exit(0);
    }
    puffin::set_scopes_on(true);
    let native_options = eframe::NativeOptions::default();
    return eframe::run_native(
        "Rusty MIPS",
        native_options,
        Box::new(|cc| Ok(Box::new(MipsIDE::new(cc)))),
    );
}

/*
fn main() {
    // Parse CLI arguments
    let cli = Cli::parse();

    // Read the assembly file
    let assembly_code = fs::read_to_string(cli.input)
        .expect("Could not read the assembly file");

    // Assemble the program into bytecode
    match assemble_program(&assembly_code) {
        Ok(bytecode) => {
            debug_println!("Assembled bytecode:");
            hexdump(&bytecode, 0, bytecode.len(), 16); // You can modify the offset, size, and bytes_per_line as needed

            // If --run flag is provided, execute the program in the VM
            if cli.run {
                debug_println!("Executing the bytecode in the VM...");
                let mut vm = VM::new();
                vm.execute(&bytecode);
            }
        },
        Err(e) => eprintln!("Error during assembly: {}", e),
    }
}
*/
