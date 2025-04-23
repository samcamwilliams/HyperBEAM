(function () {
  let currentPath = window.location.pathname;

  function updateHeaderAndMainClass() {
    const header = document.querySelector(".md-header");
    const main = document.querySelector("main");
    const isHomepage = window.location.pathname === "/index.html";

    if (!header || !main) return;

    if (isHomepage) {
      header.classList.add("custom-homepage-header");
      main.classList.add("custom-homepage-main");
      main.classList.remove("md-main");
    } else {
      header.classList.remove("custom-homepage-header");
      main.classList.remove("custom-homepage-main");
      main.classList.add("md-main");
    }
  }

  // Initial run
  updateHeaderAndMainClass();

  // Watch for URL changes
  const observer = new MutationObserver(() => {
    if (window.location.pathname !== currentPath) {
      currentPath = window.location.pathname;
      updateHeaderAndMainClass();
    }
  });

  observer.observe(document.body, { childList: true, subtree: true });

  window.addEventListener("popstate", updateHeaderAndMainClass);
})();

document.addEventListener("DOMContentLoaded", function () {
  function updateMainClass() {
    const mainElement = document.querySelector("main");
    const isHomepage = window.location.pathname === "/";

    // Apply the homepage class if on the homepage, else remove it
    if (isHomepage) {
      mainElement.classList.add("custom-homepage-main");
      mainElement.classList.remove("md-main");
    } else {
      mainElement.classList.add("md-main");
      mainElement.classList.remove("custom-homepage-main");
    }
  }

  // Initial update on page load
  updateMainClass();

  // Listen for link clicks and update the class after navigation
  const links = document.querySelectorAll("a");
  links.forEach((link) => {
    link.addEventListener("click", function (event) {
      // Small delay to ensure the page has started loading
      setTimeout(updateMainClass, 0);
    });
  });

  // Listen for popstate events (back/forward navigation)
  window.addEventListener("popstate", function () {
    setTimeout(updateMainClass, 500);
  });
});
