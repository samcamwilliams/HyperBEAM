document.addEventListener('DOMContentLoaded', function() {
  const header = document.querySelector('.md-header');
  const mainContent = document.querySelector('.md-main');

  if (!header || !mainContent) {
    // console.error('Required elements (.md-header or .md-main) not found for header scroll behavior.');
    return;
  }

  let headerHeight = header.offsetHeight;

  // Function to update paddings based on header state
  function updatePaddings() {
    // It's important to use the height of the header when it's visible
    // If header.classList.contains('header-hidden'), its offsetHeight might be its original height or 0 depending on CSS
    // For simplicity, we'll grab a fresh measurement if it's visible, or use the last known good height
    // This specific header's height doesn't change when 'header-hidden' is applied, only its transform.
    // So, header.offsetHeight should remain consistent.
    headerHeight = header.offsetHeight;

    if (header.classList.contains('header-hidden')) {
      // mainContent.style.paddingTop is handled by CSS: .header-hidden + .md-main { padding-top: 0; }
      document.documentElement.style.scrollPaddingTop = '0';
    } else {
      mainContent.style.paddingTop = headerHeight + 'px';
      document.documentElement.style.scrollPaddingTop = headerHeight + 'px';
    }
  }

  // Set initial state
  updatePaddings();

  window.addEventListener('scroll', function() {
    const scrollTop = window.pageYOffset || document.documentElement.scrollTop;

    if (scrollTop > 0) { // When scrolling down / header should be hidden
      if (!header.classList.contains('header-hidden')) {
        header.classList.add('header-hidden');
        updatePaddings(); // Update paddings after class change
      }
    } else { // When at the top / header should be visible
      if (header.classList.contains('header-hidden')) {
        header.classList.remove('header-hidden');
        updatePaddings(); // Update paddings after class change
      }
    }
  });

  window.addEventListener('resize', function() {
    // Recalculate and update paddings on resize
    updatePaddings();
  });
}); 