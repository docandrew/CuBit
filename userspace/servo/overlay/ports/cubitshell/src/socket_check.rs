/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/. */
//! Opt-in native std/libc socket endurance before the Servo engine starts.
use std::io::{Read, Write, ErrorKind};
use std::net::{SocketAddr, TcpStream};
use std::time::{Duration, Instant};

fn connect(address: SocketAddr) -> Result<TcpStream, String> {
    let socket = TcpStream::connect_timeout(&address, Duration::from_secs(10))
        .map_err(|e| format!("connect: {e}"))?;
    socket.set_nonblocking(true).map_err(|e| format!("nonblocking: {e}"))?;
    Ok(socket)
}
fn exchange(socket: &mut TcpStream, value: u64) -> Result<(), String> {
    let expected = value.to_be_bytes();
    let mut received = [0u8; 8];
    let deadline = Instant::now() + Duration::from_secs(10);
    let (mut sent, mut got) = (0, 0);
    while got < 8 {
        if sent < 8 {
            match socket.write(&expected[sent..]) {
                Ok(0) => return Err("zero write".into()),
                Ok(n) => sent += n,
                Err(e) if e.kind() == ErrorKind::WouldBlock => {},
                Err(e) => return Err(format!("write: {e}")),
            }
        }
        match socket.read(&mut received[got..]) {
            Ok(0) => return Err("unexpected EOF".into()),
            Ok(n) => got += n,
            Err(e) if e.kind() == ErrorKind::WouldBlock => {},
            Err(e) => return Err(format!("read: {e}")),
        }
        if Instant::now() >= deadline { return Err("echo deadline".into()); }
        if got < 8 { std::thread::sleep(Duration::from_millis(1)); }
    }
    if received != expected { return Err("echo content mismatch".into()); }
    Ok(())
}
pub fn run(address: SocketAddr) -> Result<(), String> {
    for index in 0..128 {
        if index % 16 == 0 { crate::say(&format!("CUBITSHELL-SOCKETS: sequential progress {index}/128")); }
        exchange(&mut connect(address)?, index).map_err(|e| format!("sequential {index}: {e}"))?;
    }
    crate::say("CUBITSHELL-SOCKETS: PASS 128 sequential lifetimes");
    let workers: Vec<_> = (0..8).map(|worker| std::thread::spawn(move || {
        for index in 0..32 {
            let mut socket = connect(address)?;
            exchange(&mut socket, 1000 + worker * 32 + index)?;
            // Exercise local close while the peer keeps the connection open.
            std::thread::sleep(Duration::from_millis(20));
        }
        Ok::<(), String>(())
    })).collect();
    let mut failure = None;
    for worker in workers {
        match worker.join() {
            Ok(Ok(())) => {},
            Ok(Err(e)) => failure = Some(e),
            Err(_) => failure = Some("worker panic".into()),
        }
    }
    if let Some(error) = failure { return Err(format!("concurrent: {error}")); }
    crate::say("CUBITSHELL-SOCKETS: PASS 256 lifetimes with 8 workers");
    std::thread::sleep(Duration::from_secs(1));
    let mut held = Vec::new();
    for index in 0..32 {
        let mut socket = connect(address).map_err(|e| format!("budget socket {index}: {e}"))?;
        exchange(&mut socket, 2000 + index)?;
        held.push(socket);
    }
    if connect(address).is_ok() { return Err("33rd connection exceeded declared budget".into()); }
    std::thread::sleep(Duration::from_secs(1));
    for (index, socket) in held.iter_mut().enumerate() { exchange(socket, 3000 + index as u64)?; }
    drop(held);
    std::thread::sleep(Duration::from_secs(1));
    for index in 0..32 { exchange(&mut connect(address)?, 4000 + index)?; }
    crate::say("CUBITSHELL-SOCKETS: PASS 32 held, excess rejected, idle reuse and recovery");
    Ok(())
}
