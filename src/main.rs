use clap::Parser;
use std::path::PathBuf;

use elm_wire3_rs::codegen;
use elm_wire3_rs::parser;

#[derive(Parser)]
#[command(name = "elm-wire3-rs")]
#[command(about = "Generate Rust types and Wire3 codecs from Elm type definitions")]
struct Cli {
    /// Path to the Elm source file to process
    input: PathBuf,

    /// Output file path (defaults to stdout)
    #[arg(short, long)]
    output: Option<PathBuf>,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let cli = Cli::parse();

    let source = std::fs::read_to_string(&cli.input)
        .map_err(|e| format!("Failed to read {}: {e}", cli.input.display()))?;

    let module = parser::parse_elm_module(&source)?;

    let rust_code = codegen::generate_rust_module(&module)?;

    match cli.output {
        Some(path) => {
            std::fs::write(&path, &rust_code)
                .map_err(|e| format!("Failed to write {}: {e}", path.display()))?;
            eprintln!("Wrote {} types to {}", module.types.len(), path.display());
        }
        None => {
            print!("{rust_code}");
        }
    }

    Ok(())
}
