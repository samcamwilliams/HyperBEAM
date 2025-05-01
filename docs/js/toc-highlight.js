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

  // Run the function whenever the hash changes (for same-page navigation)
  window.addEventListener('hashchange', updateTocHighlight);

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