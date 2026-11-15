// Apply the saved theme before first paint (prevents a flash of the wrong theme).
// sessionStorage only: nothing persists after the browser session, and no cookies are used.
(function () {
  try {
    var saved = sessionStorage.getItem('flame-theme');
    if (saved === 'dark') document.documentElement.setAttribute('data-theme', 'dark');
  } catch (e) {
    /* storage unavailable (private mode / policy): fall back to light */
  }
})();
