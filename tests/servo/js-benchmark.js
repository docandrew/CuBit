/* Fixed-work, checked JavaScript workloads shared by Penny and host browsers.
 * A microbenchmark is not a whole-browser performance score. */
(function (root) {
    "use strict";
    const workloads = [
        { id: "integer", units: 100000, run() {
            let sum = 0;
            for (let i = 0; i < 100000; ++i) sum = (sum + ((i * 17) ^ (i >>> 3))) >>> 0;
            return sum;
        } },
        { id: "typed-array", units: 524288, run() {
            const a = new Uint32Array(16384);
            for (let i = 0; i < a.length; ++i) a[i] = Math.imul(i, 2654435761) >>> 0;
            let sum = 0;
            for (let pass = 0; pass < 32; ++pass)
                for (let i = 0; i < a.length; ++i) sum = (sum + a[i]) >>> 0;
            return sum;
        } },
        { id: "objects", units: 40000, run() {
            const a = [];
            for (let i = 0; i < 2000; ++i) a.push({ value: i, tag: "v" + (i % 32) });
            let sum = 0;
            for (let pass = 0; pass < 20; ++pass)
                for (const item of a) if (item.tag.length) sum += item.value;
            return sum;
        } },
        { id: "json", units: 8000, run() {
            const a = [];
            for (let i = 0; i < 1000; ++i) a.push({ value: i, label: "row-" + i });
            let sum = 0;
            for (let pass = 0; pass < 8; ++pass)
                for (const item of JSON.parse(JSON.stringify(a))) sum += item.value;
            return sum;
        } },
        { id: "regexp", units: 2000, run() {
            let sum = 0;
            for (let i = 0; i < 2000; ++i) {
                const match = /^name-(\d+)=(\w+)$/.exec("name-" + i + "=" + i.toString(16));
                sum += Number(match[1]) + parseInt(match[2], 16);
            }
            return sum;
        } },
        { id: "sort", units: 16384, run() {
            const a = [];
            for (let i = 0; i < 4096; ++i) a.push((i * 4051) % 4096);
            let sum = 0;
            for (let pass = 0; pass < 4; ++pass) {
                const sorted = a.slice().sort((a, b) => a - b);
                for (let i = 0; i < sorted.length; ++i) sum = (sum + (i + 1) * sorted[i]) >>> 0;
            }
            return sum;
        } },
    ];
    async function run(expected, reportSample = () => {}) {
        const report = { version: "penny-js-v1", warmups: 1, repeats: 5, results: [] };
        for (const workload of workloads) {
            const samples = [];
            for (let repeat = -1; repeat < report.repeats; ++repeat) {
                // Let input/paint run between samples. Work counts remain fixed.
                await new Promise(resolve => setTimeout(resolve, 0));
                const start = performance.now();
                const checksum = workload.run();
                const elapsed = performance.now() - start;
                if (checksum !== expected[workload.id]) throw new Error(workload.id + " checksum mismatch: " + checksum);
                if (!(elapsed >= 0 && Number.isFinite(elapsed))) throw new Error("invalid clock sample");
                if (repeat >= 0) {
                    samples.push(elapsed);
                    reportSample(workload.id, repeat, elapsed, checksum);
                }
            }
            const sorted = samples.slice().sort((a, b) => a - b);
            report.results.push({ id: workload.id, units: workload.units, checksum: expected[workload.id],
                samples_ms: samples, median_ms: sorted[2], minimum_ms: sorted[0], maximum_ms: sorted[4] });
        }
        return report;
    }
    root.PennyJSBenchmark = { workloads, run };
})(globalThis);
