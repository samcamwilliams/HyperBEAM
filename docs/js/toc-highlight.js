document.addEventListener('DOMContentLoaded', function() {
  // Function to update ToC highlighting
  function updateTocHighlighting() {
    // Get all active links in the secondary navigation
    const activeLinks = document.querySelectorAll('.md-nav--secondary .md-nav__link--active');
    
    activeLinks.forEach(activeLink => {
      // Find the parent list item
      const parentItem = activeLink.closest('li.md-nav__item--active');
      if (parentItem) {
        // Find the parent link and remove its active state
        const parentLink = parentItem.querySelector(':scope > label.md-nav__link');
        if (parentLink) {
          parentLink.classList.remove('md-nav__link--active');
        }
      }
    });
  }
  
  // Update on scroll with throttling to improve performance
  let scrollTimeout;
  window.addEventListener('scroll', function() {
    if (!scrollTimeout) {
      scrollTimeout = setTimeout(function() {
        updateTocHighlighting();
        scrollTimeout = null;
      }, 50);
    }
  });
  
  // Initial update
  updateTocHighlighting();
  
  // Create a mutation observer to detect when the active class changes
  const observer = new MutationObserver(function(mutations) {
    mutations.forEach(function(mutation) {
      if (mutation.type === 'attributes' && 
          mutation.attributeName === 'class' &&
          mutation.target.classList.contains('md-nav__link--active')) {
        updateTocHighlighting();
      }
    });
  });
  
  // Observe all nav links for class changes
  document.querySelectorAll('.md-nav__link').forEach(link => {
    observer.observe(link, { attributes: true });
  });
  
  // Update on click of ToC items
  document.addEventListener('click', function(e) {
    if (e.target.closest('.md-nav__link')) {
      // Small delay to allow the page to update classes
      setTimeout(updateTocHighlighting, 50);
    }
  });
});