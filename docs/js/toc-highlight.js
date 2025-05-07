document.addEventListener('DOMContentLoaded', function() {
  const tocLinksQuery = '.md-nav--secondary .md-nav__link';

  function getHeaderHeight() {
    const header = document.querySelector('.md-header');
    if (header && getComputedStyle(header).position === 'fixed' || getComputedStyle(header).position === 'sticky') {
      return header.offsetHeight;
    }
    return 0;
  }

  function smoothScrollTo(targetId, duration = 350) { // Fast duration
    const targetElement = document.getElementById(targetId.substring(1)); // Remove #
    if (!targetElement) return;

    const headerOffset = getHeaderHeight();
    const targetPosition = targetElement.getBoundingClientRect().top + window.scrollY - headerOffset;
    const startPosition = window.scrollY;
    const distance = targetPosition - startPosition;
    let startTime = null;

    function animation(currentTime) {
      if (startTime === null) startTime = currentTime;
      const timeElapsed = currentTime - startTime;
      const progress = Math.min(timeElapsed / duration, 1);
      // Apply cubic-bezier like easing. For t in [0, 1], use a common curve.
      // Using a more standard one:
      const t = progress;
      const easedProgress = t < 0.5 ? 4 * t * t * t : 1 - Math.pow(-2 * t + 2, 3) / 2; // easeInOutCubic

      window.scrollTo(0, startPosition + distance * easedProgress);

      if (timeElapsed < duration) {
        requestAnimationFrame(animation);
      } else {
        // Ensure the hash is updated after the scroll completes for highlighting and history.
        if (window.location.hash !== targetId) {
          window.location.hash = targetId;         // This will trigger the 'hashchange' event once
        }
        // The 'hashchange' event listener will call updateTocHighlight.
      }
    }
    requestAnimationFrame(animation);
  }

  function updateTocHighlight() {
    // Remove existing active classes from ToC links
    const tocLinks = document.querySelectorAll(tocLinksQuery);
    tocLinks.forEach(link => {
      link.classList.remove('md-nav__link--active');
    });

    // Get the current hash, decoded
    const currentHash = window.location.hash;
    if (currentHash) {
      try {
        const decodedHash = decodeURIComponent(currentHash);
        // Find the ToC link that matches the current hash
        // We specifically target links within the ToC nav (.md-nav--secondary)
        const targetLink = document.querySelector(tocLinksQuery + '[href$="' + decodedHash + '"]');

        if (targetLink) {
          targetLink.classList.add('md-nav__link--active');
        } else {
          // No specific link found for the hash, which can be normal
        }
      } catch (e) {
          console.error("Error decoding hash for TOC highlight:", e);
      }
    }
  }

  // Run the function on initial page load
  updateTocHighlight();

  // Run the function whenever the hash changes (for same-page navigation)
  window.addEventListener('hashchange', updateTocHighlight);

  // Add click listeners for smooth scroll
  document.querySelectorAll(tocLinksQuery).forEach(link => {
    link.addEventListener('click', function(event) {
      const href = this.getAttribute('href');
      if (href && href.startsWith('#')) {
        event.preventDefault();
        smoothScrollTo(href);
      }
    });
  });

  // Compatibility with MkDocs Material Instant Loading:
  // Subscribe to the document$ observable which emits after instant loading completes.
  if (typeof document$ !== 'undefined') {
    document$.subscribe(function() {
      // Use a small timeout to ensure the DOM is fully updated after navigation
      setTimeout(updateTocHighlight, 50);
    });
  } else {
    console.warn("MkDocs Material 'document$' observable not found. Instant loading TOC highlighting might not work.");
    // Fallback or alternative logic could be placed here if needed,
    // but relying on document$ is the primary method for instant loading.
  }

}); 