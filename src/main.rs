use std::env;
use std::process;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        let program = args.get(0).map(String::as_str).unwrap_or("ochicc");
        eprintln!("usage: {program} <integer>");
        process::exit(1);
    }

    let value_str = &args[1];
    let value: i64 = match value_str.parse() {
        Ok(num) => num,
        Err(_) => {
            eprintln!("not an integer: {value_str}");
            process::exit(1);
        }
    };

    println!(".global main");
    println!("main:");
    println!("    movl ${value}, %eax");
    println!("    ret");
}
