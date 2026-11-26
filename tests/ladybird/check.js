// SPDX-License-Identifier: GPL-3.0-or-later
// Ladybird's js on CuBit (WEB-006 phase a): a little of each thing LibJS
// leans on the platform for: the GC's heap region and the typed-array cage
// (reserved address space, committed in place), the regex engine, ICU's
// data (Intl, normalization), the clock, and the job queue. Each line is
// "ladybird-js: <check> PASS|FAIL"; the last is LADYBIRD-JS: PASS or FAIL.
const results = [];
function check(name, ok) {
    console.log(`ladybird-js: ${name} ${ok ? "PASS" : "FAIL"}`);
    results.push(ok);
}

check("arithmetic and strings", (6 * 7).toString() === "42" && "Lady" + "bird" === "Ladybird");

const kept = [];
for (let i = 0; i < 200000; i++) {
    const object = { i, text: "x" + i };
    if (i % 1000 === 0)
        kept.push(object);
}
check("allocation and collection", kept.length === 200 && kept[199].text === "x199000");

const buffer = new ArrayBuffer(1 << 20);
const words = new Uint32Array(buffer);
words.fill(7);
check("ArrayBuffer and typed arrays", words.reduce((sum, word) => sum + word, 0) === 7 * (1 << 18));
const resizable = new ArrayBuffer(16, { maxByteLength: 1 << 24 });
resizable.resize(1 << 23);
check("resizable ArrayBuffer", resizable.byteLength === 1 << 23 && new Uint8Array(resizable)[(1 << 23) - 1] === 0);

check("RegExp", /(\d+)-(\d+)/.exec("10-20")[2] === "20" && "aXbx".replace(/x/gi, "_") === "a_b_");
check("RegExp Unicode properties", /\p{Script=Greek}/u.test("α") && !/\p{Script=Greek}/u.test("a"));
check("JSON", JSON.stringify(JSON.parse('{"a":[1,2,{"b":null}]}')) === '{"a":[1,2,{"b":null}]}');
check("BigInt", (2n ** 100n).toString() === "1267650600228229401496703205376");
check("Intl.NumberFormat", new Intl.NumberFormat("de-DE").format(1234567.891) === "1.234.567,891");
check("Intl.DateTimeFormat",
    new Intl.DateTimeFormat("en-US", { timeZone: "UTC", year: "numeric", month: "long" }).format(new Date(0)) === "January 1970");
check("Intl.Collator", ["b", "a", "ä"].sort(new Intl.Collator("de").compare).join("") === "aäb");
check("String.prototype.normalize", "Å".normalize("NFC") === "Å");
check("Date", new Date(Date.UTC(2026, 10, 26)).toISOString() === "2026-11-26T00:00:00.000Z" && Date.now() > 0);
check("Math", Math.abs(Math.sin(Math.PI / 6) - 0.5) < 1e-15 && Math.hypot(3, 4) === 5);

const order = [];
Promise.resolve().then(() => {
    order.push("job");
    check("promise jobs", order.join() === "script,job");
    (async () => {
        await null;
        return 5;
    })().then(value => {
        check("async functions", value === 5);
        console.log(results.every(ok => ok) ? "LADYBIRD-JS: PASS" : "LADYBIRD-JS: FAIL");
    });
});
order.push("script");
