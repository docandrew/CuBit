// WEB-001 compatibility survey: the per-load summary, evaluated after the
// settle period in every engine. Returns one JSON string. Times are
// milliseconds from the navigation's time origin; null when the engine does
// not report them.
(() => {
  const MAXIMUM_SAMPLE_CHARACTERS = 300;
  const MAXIMUM_TITLE_CHARACTERS = 120;
  const state = window.__cubitCompat || null;
  const entry = (type) => (performance.getEntriesByType ? performance.getEntriesByType(type) : []);
  const paint = (name) => {
    const found = entry("paint").find((e) => e.name === name);
    return found ? Math.round(found.startTime) : null;
  };
  const navigation = entry("navigation")[0] || null;
  let load = navigation && navigation.loadEventEnd > 0 ? navigation.loadEventEnd : null;
  let contentLoaded = navigation && navigation.domContentLoadedEventEnd > 0
    ? navigation.domContentLoadedEventEnd : null;
  const timing = performance.timing;
  if (load === null && timing && timing.loadEventEnd > 0) {
    load = timing.loadEventEnd - timing.navigationStart;
  }
  if (contentLoaded === null && timing && timing.domContentLoadedEventEnd > 0) {
    contentLoaded = timing.domContentLoadedEventEnd - timing.navigationStart;
  }
  // The document's own fetch, by phase (Navigation Timing); null where the
  // engine reports no phase (a reused connection has no DNS, connect or TLS).
  const phase = (from, to) => (navigation && navigation[from] > 0 && navigation[to] >= navigation[from]
    ? Math.round(navigation[to] - navigation[from]) : null);
  const network = navigation ? {
    redirect: phase("redirectStart", "redirectEnd"),
    dns: phase("domainLookupStart", "domainLookupEnd"),
    connect: phase("connectStart", "connectEnd"),
    tls: phase("secureConnectionStart", "connectEnd"),
    firstByte: phase("requestStart", "responseStart"),
    response: phase("responseStart", "responseEnd"),
    fetchStart: Math.round(navigation.fetchStart),
    responseEnd: Math.round(navigation.responseEnd),
    documentBytes: navigation.encodedBodySize || null,
    protocol: navigation.nextHopProtocol || null,
  } : null;
  const resources = entry("resource");
  const resourceBytes = resources.reduce((sum, r) => sum + (r.encodedBodySize || 0), 0);
  const body = document.body;
  const text = body ? (body.innerText || "").replace(/\s+/g, " ").trim() : "";
  const round = (value) => (value === null || value === undefined ? null : Math.round(value));
  return JSON.stringify({
    probe: state !== null,
    firstPaint: paint("first-paint"),
    firstContentfulPaint: paint("first-contentful-paint"),
    contentLoaded: round(contentLoaded),
    load: round(load),
    textAt: state ? round(state.textAt) : null,
    textCharacters: text.replace(/ /g, "").length,
    errors: state ? state.errors : [],
    errorCount: state ? state.errorCount : 0,
    consoleErrors: state ? state.consoleErrors : 0,
    readyState: document.readyState,
    title: document.title.slice(0, MAXIMUM_TITLE_CHARACTERS),
    origin: location.origin,
    elements: document.getElementsByTagName("*").length,
    images: document.images.length,
    resources: resources.length,
    resourceBytes,
    network,
    sample: text.slice(0, MAXIMUM_SAMPLE_CHARACTERS),
    // What the page can see of the browser: sites choose layouts, consent
    // banners and app prompts from these.
    environment: {
      userAgent: navigator.userAgent,
      platform: navigator.platform || null,
      language: navigator.language || null,
      timeZone: (() => { try { return Intl.DateTimeFormat().resolvedOptions().timeZone; } catch (e) { return null; } })(),
      timezoneOffset: new Date().getTimezoneOffset(),
      touch: navigator.maxTouchPoints || 0,
      cookies: document.cookie.length,
    },
  });
})()
