use std::env;
use std::process;

fn main() {
  let args: Vec<String> = env::args().collect();
  if args.len() != 2 {
    let program = args.first().map(String::as_str).unwrap_or("rchicc");
    eprintln!("usage: {program} <expr>");
    process::exit(1);
  }

  match rchicc::generate_assembly(&args[1]) {
    Ok(asm) => print!("{asm}"),
    Err(err) => {
      eprintln!("{err}");
      process::exit(1);
    }
  }
}
