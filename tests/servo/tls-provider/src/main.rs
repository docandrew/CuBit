//! Penny's SPARK rustls provider (penny-crypto), hosted on Linux:
//! - known-answer digests and HMACs;
//! - a handshake matrix against rustls's aws-lc provider as the server:
//!   TLS 1.3 and 1.2, every suite, every group (X25519MLKEM768, X25519,
//!   P-256, P-384), RSA, ECDSA P-256/P-384 and Ed25519 chains, data both
//!   ways; a P-521 chain must be refused;
//! - with --bench: handshake and bulk throughput of the client, penny-crypto
//!   against aws-lc (the same aws-lc server), medians with spread.
use std::io::{Read, Write};
use std::sync::Arc;
use std::time::Instant;

use rcgen::{BasicConstraints, CertificateParams, IsCa, KeyPair, SignatureAlgorithm};
use rustls::crypto::{CryptoProvider, SupportedKxGroup, aws_lc_rs};
use rustls::pki_types::{CertificateDer, PrivateKeyDer, PrivatePkcs8KeyDer, ServerName};
use rustls::{ClientConfig, ClientConnection, RootCertStore, ServerConfig, ServerConnection, SupportedCipherSuite};

fn hex(bytes: &[u8]) -> String {
    bytes.iter().map(|b| format!("{b:02x}")).collect()
}

fn known_answers(failures: &mut Vec<String>) {
    let checks: [(&str, String, &str); 6] = [
        ("SHA-1 abc", hex(&penny_crypto::sha1(b"abc").unwrap()), "a9993e364706816aba3e25717850c26c9cd0d89d"),
        ("SHA-256 abc", hex(&penny_crypto::sha256(b"abc").unwrap()),
         "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"),
        ("SHA-384 abc", hex(&penny_crypto::sha384(b"abc").unwrap()),
         "cb00753f45a35e8bb5a03d699ac65007272c32ab0eded1631a8b605a43ff5bed8086072ba1e7cc2358baeca134c825a7"),
        ("SHA-512 abc", hex(&penny_crypto::sha512(b"abc").unwrap()),
         "ddaf35a193617abacc417349ae20413112e6fa4e89a97ea20a9eeee64b55d39a2192992a274fc1a836ba3c23a3feebbd454d4423643ce80e2a9ac94fa54ca49f"),
        ("HMAC-SHA-256 RFC 4231 2", hex(&penny_crypto::hmac_sha256(b"Jefe", b"what do ya want for nothing?").unwrap()),
         "5bdcc146bf60754e6a042426089575c75a003f089d2739839dec58b964ec3843"),
        ("HMAC-SHA-384 RFC 4231 2", hex(&penny_crypto::hmac_sha384(b"Jefe", b"what do ya want for nothing?").unwrap()),
         "af45d2e376484031617f78d2b58a6b1b9c7ef464f5a01b47e42ec3736322445e8e2240ca5e69e2c78b3239ecfab21649"),
    ];
    for (name, got, want) in checks {
        if got != want {
            failures.push(format!("{name}: {got}, want {want}"));
        }
    }
}

struct Chain {
    name: &'static str,
    root: CertificateDer<'static>,
    chain: Vec<CertificateDer<'static>>,
    key: PrivateKeyDer<'static>,
}

fn chain(name: &'static str, algorithm: &'static SignatureAlgorithm) -> Chain {
    mixed_chain(name, algorithm, algorithm)
}

/// A CA of one key type over a leaf of another.
fn mixed_chain(name: &'static str, ca_algorithm: &'static SignatureAlgorithm,
               algorithm: &'static SignatureAlgorithm) -> Chain {
    let ca_key = KeyPair::generate_for(ca_algorithm).expect("CA key");
    let mut ca = CertificateParams::new(Vec::<String>::new()).unwrap();
    ca.is_ca = IsCa::Ca(BasicConstraints::Unconstrained);
    ca.distinguished_name.push(rcgen::DnType::CommonName, format!("Penny test CA {name}"));
    let ca_cert = ca.self_signed(&ca_key).unwrap();
    let leaf_key = KeyPair::generate_for(algorithm).expect("leaf key");
    let leaf = CertificateParams::new(vec!["penny.test".to_string()]).unwrap()
        .signed_by(&leaf_key, &ca_cert, &ca_key).unwrap();
    Chain {
        name,
        root: ca_cert.der().clone(),
        chain: vec![leaf.der().clone(), ca_cert.der().clone()],
        key: PrivateKeyDer::Pkcs8(PrivatePkcs8KeyDer::from(leaf_key.serialize_der())),
    }
}

fn server_config(chain: &Chain, suite: SupportedCipherSuite, group: &'static dyn SupportedKxGroup) -> Arc<ServerConfig> {
    let provider = CryptoProvider { cipher_suites: vec![suite], kx_groups: vec![group], ..aws_lc_rs::default_provider() };
    let version = match suite {
        SupportedCipherSuite::Tls13(_) => &rustls::version::TLS13,
        SupportedCipherSuite::Tls12(_) => &rustls::version::TLS12,
    };
    Arc::new(ServerConfig::builder_with_provider(Arc::new(provider))
        .with_protocol_versions(&[version]).unwrap()
        .with_no_client_auth()
        .with_single_cert(chain.chain.clone(), chain.key.clone_key()).expect("server certificate"))
}

fn client_config(provider: CryptoProvider, root: &CertificateDer<'static>) -> Arc<ClientConfig> {
    let mut roots = RootCertStore::empty();
    roots.add(root.clone()).unwrap();
    let mut config = ClientConfig::builder_with_provider(Arc::new(provider))
        .with_protocol_versions(&[&rustls::version::TLS13, &rustls::version::TLS12]).unwrap()
        .with_root_certificates(roots).with_no_client_auth();
    // Every connection a full handshake (no resumption across the matrix).
    config.resumption = rustls::client::Resumption::disabled();
    Arc::new(config)
}

/// Pump records between the two until neither has anything to send.
fn pump(client: &mut ClientConnection, server: &mut ServerConnection) -> Result<(), String> {
    for _ in 0..1000 {
        let mut moved = false;
        let mut buf = Vec::new();
        while client.wants_write() {
            client.write_tls(&mut buf).map_err(|e| e.to_string())?;
        }
        if !buf.is_empty() {
            moved = true;
            let mut slice = &buf[..];
            while !slice.is_empty() {
                server.read_tls(&mut slice).map_err(|e| e.to_string())?;
                server.process_new_packets().map_err(|e| format!("server: {e}"))?;
            }
        }
        buf.clear();
        while server.wants_write() {
            server.write_tls(&mut buf).map_err(|e| e.to_string())?;
        }
        if !buf.is_empty() {
            moved = true;
            let mut slice = &buf[..];
            while !slice.is_empty() {
                client.read_tls(&mut slice).map_err(|e| e.to_string())?;
                client.process_new_packets().map_err(|e| format!("client: {e}"))?;
            }
        }
        if !moved {
            return Ok(());
        }
    }
    Err("no progress".into())
}

fn connect(client: &Arc<ClientConfig>, server: &Arc<ServerConfig>) -> Result<(ClientConnection, ServerConnection), String> {
    let mut c = ClientConnection::new(client.clone(), ServerName::try_from("penny.test").unwrap()).unwrap();
    let mut s = ServerConnection::new(server.clone()).unwrap();
    c.set_buffer_limit(None);
    s.set_buffer_limit(None);
    pump(&mut c, &mut s)?;
    if c.is_handshaking() || s.is_handshaking() {
        return Err("handshake did not finish".into());
    }
    Ok((c, s))
}

fn exchange(c: &mut ClientConnection, s: &mut ServerConnection, size: usize) -> Result<(), String> {
    let up: Vec<u8> = (0..size).map(|i| (i * 7) as u8).collect();
    let down: Vec<u8> = (0..size).map(|i| (i * 13) as u8).collect();
    // In chunks: rustls holds at most 16 KiB of unread plaintext.
    const CHUNK: usize = 8000;
    let (mut got_up, mut got_down) = (vec![0u8; size], vec![0u8; size]);
    for start in (0..size).step_by(CHUNK) {
        let end = (start + CHUNK).min(size);
        c.writer().write_all(&up[start..end]).unwrap();
        s.writer().write_all(&down[start..end]).unwrap();
        pump(c, s)?;
        s.reader().read_exact(&mut got_up[start..end]).map_err(|e| format!("server read: {e}"))?;
        c.reader().read_exact(&mut got_down[start..end]).map_err(|e| format!("client read: {e}"))?;
    }
    if got_up != up || got_down != down {
        return Err("data differs".into());
    }
    Ok(())
}

fn matrix(failures: &mut Vec<String>) {
    let chains = [
        chain("RSA-2048", &rcgen::PKCS_RSA_SHA256),
        chain("ECDSA P-256", &rcgen::PKCS_ECDSA_P256_SHA256),
        chain("ECDSA P-384", &rcgen::PKCS_ECDSA_P384_SHA384),
        chain("Ed25519", &rcgen::PKCS_ED25519),
    ];
    let groups: [(&str, &'static dyn SupportedKxGroup); 4] = [
        ("X25519MLKEM768", aws_lc_rs::kx_group::X25519MLKEM768),
        ("X25519", aws_lc_rs::kx_group::X25519),
        ("P-256", aws_lc_rs::kx_group::SECP256R1),
        ("P-384", aws_lc_rs::kx_group::SECP384R1),
    ];
    let mut passed = 0;
    for chain in &chains {
        let client = client_config(penny_crypto::provider(), &chain.root);
        for &suite in aws_lc_rs::ALL_CIPHER_SUITES {
            let tls12 = matches!(suite, SupportedCipherSuite::Tls12(_));
            let name = format!("{:?}", suite.suite());
            if tls12 && (name.contains("ECDSA") != (chain.name != "RSA-2048")) {
                continue; // a TLS 1.2 suite names its certificate type
            }
            for (group_name, group) in groups {
                if tls12 && group_name == "X25519MLKEM768" {
                    continue;
                }
                let label = format!("{} {} {}", chain.name, name, group_name);
                match connect(&client, &server_config(chain, suite, group)).and_then(|(mut c, mut s)| {
                    if c.negotiated_cipher_suite() != Some(suite) {
                        return Err(format!("negotiated {:?}", c.negotiated_cipher_suite()));
                    }
                    exchange(&mut c, &mut s, 40_000)
                }) {
                    Ok(()) => passed += 1,
                    Err(error) => failures.push(format!("{label}: {error}")),
                }
            }
        }
    }
    println!("handshake matrix: {passed} combinations passed");
    // P-521: no SPARK verifier. A P-521 server key leaves no signature
    // scheme in common; a P-521 CA over a P-256 leaf is a certificate error.
    let suite = aws_lc_rs::cipher_suite::TLS13_AES_128_GCM_SHA256;
    for (label, p521, expected) in [
        ("P-521 server key", chain("ECDSA P-521", &rcgen::PKCS_ECDSA_P521_SHA512), "NoSignatureSchemesInCommon"),
        ("P-521 CA", mixed_chain("P-521 CA", &rcgen::PKCS_ECDSA_P521_SHA512, &rcgen::PKCS_ECDSA_P256_SHA256),
         "UnsupportedSignatureAlgorithm"),
    ] {
        let client = client_config(penny_crypto::provider(), &p521.root);
        match connect(&client, &server_config(&p521, suite, aws_lc_rs::kx_group::X25519)) {
            Ok(_) => failures.push(format!("{label}: accepted")),
            Err(error) if error.contains(expected) => println!("{label}: refused ({error})"),
            Err(error) => failures.push(format!("{label}: refused, but not with {expected}: {error}")),
        }
    }
}

fn median(values: &mut [f64]) -> (f64, f64, f64) {
    values.sort_by(|a, b| a.partial_cmp(b).unwrap());
    (values[values.len() / 2], values[0], values[values.len() - 1])
}

fn bench() {
    let chain = chain("ECDSA P-256", &rcgen::PKCS_ECDSA_P256_SHA256);
    const REPEATS: usize = 7;
    const HANDSHAKES: usize = 200;
    const BULK: usize = 64 << 20;
    for (group_name, group) in [("X25519", aws_lc_rs::kx_group::X25519), ("X25519MLKEM768", aws_lc_rs::kx_group::X25519MLKEM768)] {
        for (provider_name, provider) in [("penny-crypto", penny_crypto::provider as fn() -> CryptoProvider),
                                          ("aws-lc", aws_lc_rs::default_provider as fn() -> CryptoProvider)] {
            let client = client_config(provider(), &chain.root);
            let server = server_config(&chain, aws_lc_rs::cipher_suite::TLS13_AES_128_GCM_SHA256, group);
            let mut rates: Vec<f64> = (0..REPEATS).map(|_| {
                let start = Instant::now();
                for _ in 0..HANDSHAKES {
                    connect(&client, &server).unwrap();
                }
                HANDSHAKES as f64 / start.elapsed().as_secs_f64()
            }).collect();
            let (m, lo, hi) = median(&mut rates);
            println!("handshakes/s {group_name} TLS1.3 ECDSA-P256, {provider_name} client + aws-lc server: median {m:.0} (min {lo:.0}, max {hi:.0})");
        }
    }
    for suite in [aws_lc_rs::cipher_suite::TLS13_AES_128_GCM_SHA256, aws_lc_rs::cipher_suite::TLS13_AES_256_GCM_SHA384,
                  aws_lc_rs::cipher_suite::TLS13_CHACHA20_POLY1305_SHA256] {
        for (provider_name, provider) in [("penny-crypto", penny_crypto::provider as fn() -> CryptoProvider),
                                          ("aws-lc", aws_lc_rs::default_provider as fn() -> CryptoProvider)] {
            let client = client_config(provider(), &chain.root);
            let server = server_config(&chain, suite, aws_lc_rs::kx_group::X25519);
            let (mut seal, mut open) = (Vec::new(), Vec::new());
            for _ in 0..REPEATS {
                let (mut c, mut s) = connect(&client, &server).unwrap();
                let data = vec![0x5au8; BULK];
                // Client seals (upload): time the client's writes only.
                let start = Instant::now();
                let mut wire = Vec::with_capacity(BULK + BULK / 64);
                c.writer().write_all(&data).unwrap();
                while c.wants_write() {
                    c.write_tls(&mut wire).unwrap();
                }
                seal.push(BULK as f64 / start.elapsed().as_secs_f64() / 1e6);
                // Server seals; client opens (download): time the client's reads.
                s.writer().write_all(&data).unwrap();
                let mut wire = Vec::with_capacity(BULK + BULK / 64);
                while s.wants_write() {
                    s.write_tls(&mut wire).unwrap();
                }
                let start = Instant::now();
                let mut slice = &wire[..];
                let mut sink = vec![0u8; 1 << 16];
                let mut got = 0;
                while !slice.is_empty() || got < BULK {
                    if !slice.is_empty() {
                        c.read_tls(&mut slice).unwrap();
                        c.process_new_packets().unwrap();
                    }
                    match c.reader().read(&mut sink) {
                        Ok(n) => got += n,
                        Err(_) => {},
                    }
                }
                open.push(BULK as f64 / start.elapsed().as_secs_f64() / 1e6);
            }
            let ((sm, sl, sh), (om, ol, oh)) = (median(&mut seal), median(&mut open));
            println!("bulk {:?} {provider_name}: seal median {sm:.0} MB/s ({sl:.0}..{sh:.0}), open median {om:.0} MB/s ({ol:.0}..{oh:.0})",
                     suite.suite());
        }
    }
}

fn main() {
    if std::env::args().any(|a| a == "--bench") {
        bench();
        return;
    }
    let mut failures = Vec::new();
    known_answers(&mut failures);
    matrix(&mut failures);
    for failure in &failures {
        println!("FAIL {failure}");
    }
    println!("PENNY-TLS-PROVIDER: {}", if failures.is_empty() { "PASS" } else { "FAIL" });
    std::process::exit(if failures.is_empty() { 0 } else { 1 });
}
