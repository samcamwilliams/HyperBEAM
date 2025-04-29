document.addEventListener('DOMContentLoaded', function() {
  function updateTocHighlight() {
    // Remove existing active classes from ToC links
    const tocLinks = document.querySelectorAll('.md-nav--secondary .md-nav__link');
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
        const targetLink = document.querySelector(`.md-nav--secondary .md-nav__link[href$="${decodedHash}"]`);

        if (targetLink) {
          targetLink.classList.add('md-nav__link--active');
          // console.log('TOC highlight added to:', targetLink.href); // For debugging
        } else {
          // console.log('TOC highlight: No link found for hash:', decodedHash); // For debugging
        }
      } catch (e) {
          console.error("Error decoding hash for TOC highlight:", e);
      }
    }
  }

  // Run the function on initial page load
  updateTocHighlight();

  // Run the function whenever the hash changes
  window.addEventListener('hashchange', updateTocHighlight);

  // Compatibility with MkDocs Material Instant Loading:
  // Re-run the function after instant loading navigation completes.
  // We need to observe mutations on the main content area or use RxJS if available.
  // A simpler approach for now: Re-run on any click within the document,
  // potentially debounced or throttled if performance becomes an issue.
  // However, 'hashchange' should cover most cases. Let's add an observer
  // for content changes as a fallback for instant loading edge cases.

  const contentElement = document.querySelector('.md-content'); // Adjust selector if needed
  if (contentElement) {
    const observer = new MutationObserver(mutations => {
      // We only care that *something* changed, possibly due to instant loading.
      // Check if the hash is different or if active link needs update.
      // Debounce or throttle this if it fires too often.
      // A simple timeout might suffice to avoid running too many times during transitions.
      setTimeout(updateTocHighlight, 100); // Small delay
    });

    observer.observe(contentElement, { childList: true, subtree: true });
  } else {
    // Fallback if specific content element isn't found, listen more broadly (less efficient)
     const bodyObserver = new MutationObserver(mutations => {
       setTimeout(updateTocHighlight, 100);
     });
     bodyObserver.observe(document.body, { childList: true, subtree: true });
  }

}); 