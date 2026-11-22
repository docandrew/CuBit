// WEB-001 compatibility survey probe. Every engine installs it before the
// page's own scripts (Servo: a UserScript; Playwright: addInitScript), in the
// top-level document only. It records script errors, console.error calls and
// the first moment the page shows visible text, for collect.js to read.
(() => {
  if (window !== window.top || window.__cubitCompat) {
    return;
  }
  // Visible text: at least this many non-space characters of the body's
  // rendered text (innerText skips hidden content).
  const MINIMUM_TEXT_CHARACTERS = 40;
  const POLL_MILLISECONDS = 50;
  const MAXIMUM_KEPT_ERRORS = 20;
  const MAXIMUM_MESSAGE_CHARACTERS = 200;
  const state = {
    errors: [],
    errorCount: 0,
    consoleErrors: 0,
    textAt: null,
  };
  Object.defineProperty(window, "__cubitCompat", { value: state, enumerable: false });
  const note = (kind, message) => {
    state.errorCount += 1;
    if (state.errors.length < MAXIMUM_KEPT_ERRORS) {
      state.errors.push(kind + ": " + String(message).slice(0, MAXIMUM_MESSAGE_CHARACTERS));
    }
  };
  window.addEventListener("error", (event) => {
    // Script errors only; failed resource loads are ordinary Events.
    if (event instanceof ErrorEvent) {
      note("error", event.message);
    }
  }, true);
  window.addEventListener("unhandledrejection", (event) => {
    const reason = event.reason;
    note("rejection", reason && reason.message ? reason.message : reason);
  });
  const consoleError = console.error;
  console.error = function (...values) {
    state.consoleErrors += 1;
    return consoleError.apply(this, values);
  };
  const visibleCharacters = () => {
    const body = document.body;
    return body ? (body.innerText || "").replace(/\s+/g, "").length : 0;
  };
  const poll = () => {
    if (visibleCharacters() >= MINIMUM_TEXT_CHARACTERS) {
      state.textAt = performance.now();
      return;
    }
    setTimeout(poll, POLL_MILLISECONDS);
  };
  poll();
})();
