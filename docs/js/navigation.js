// navigation.js - Smooth scrolling navigation with proper URL updating

// Main function to handle scrolling to a heading
function scrollToHeading(headingId, event) {
  // Prevent default hash change behavior
  if (event) event.preventDefault();

  // Get the element by ID without the # symbol
  const element = document.getElementById(headingId);

  if (element) {
    // Scroll to the element smoothly
    element.scrollIntoView({ behavior: 'smooth' });

    // Update URL without triggering another navigation event
    history.pushState(null, null, `#${headingId}`);

    // Always force update the active TOC item, regardless of scroll position
    forceUpdateActiveTocItem(headingId);
  }
}

// Force update the active TOC item (used for direct clicks)
function forceUpdateActiveTocItem(headingId) {
  // Remove active class from all TOC links
  document.querySelectorAll('.md-nav--secondary a').forEach(link => {
    link.classList.remove('md-nav__link--active');
  });

  // Add active class to the current TOC link
  const currentTocItem = document.querySelector(`.md-nav--secondary a[href="#${headingId}"]`);
  if (currentTocItem) {
    currentTocItem.classList.add('md-nav__link--active');

    // If the item is not visible in the TOC viewport, scroll to it
    const tocContainer = document.querySelector('.md-nav--secondary');
    if (tocContainer) {
      const itemRect = currentTocItem.getBoundingClientRect();
      const containerRect = tocContainer.getBoundingClientRect();

      if (itemRect.bottom > containerRect.bottom || itemRect.top < containerRect.top) {
        currentTocItem.scrollIntoView({ behavior: 'smooth', block: 'nearest' });
      }
    }
  }
}

// Determine which heading is currently in view during scrolling
function updateTocOnScroll() {
  // Get all headings that might be in the TOC
  const headings = Array.from(document.querySelectorAll('h1, h2, h3, h4, h5, h6')).filter(heading => heading.id);
  if (headings.length === 0) return;

  // Set a small offset to consider a heading "active" before it reaches the top
  const scrollOffset = 100;

  // Special handling for bottom of page
  const scrollBottom = window.scrollY + window.innerHeight;
  const docHeight = document.documentElement.scrollHeight;

  // If we're at or very near the bottom of the page, activate the last heading
  if (scrollBottom >= docHeight - 50) {
    forceUpdateActiveTocItem(headings[headings.length - 1].id);
    return;
  }

  // Find the heading that's currently in view
  let activeHeading = null;

  for (let i = 0; i < headings.length; i++) {
    const heading = headings[i];
    const rect = heading.getBoundingClientRect();

    // Check if this heading is near the top of the viewport
    if (rect.top <= scrollOffset) {
      activeHeading = heading;
    } else {
      // We've found the first heading below our threshold, so the previous one is active
      break;
    }
  }

  // If we found an active heading, update the TOC
  if (activeHeading) {
    forceUpdateActiveTocItem(activeHeading.id);
  } else if (headings.length > 0 && window.scrollY < 100) {
    // If we're at the top of the page, activate the first heading
    forceUpdateActiveTocItem(headings[0].id);
  }
}

// Initialize navigation functionality
function initNavigation() {
  // Get all navigation links
  const navLinks = document.querySelectorAll('a[href^="#"]');

  // Add click event listeners to all navigation links
  navLinks.forEach(link => {
    link.addEventListener('click', function(event) {
      // Always prevent default behavior first
      event.preventDefault();

      // Extract the heading ID from the href attribute
      const headingId = this.getAttribute('href').substring(1);

      // Only proceed if heading ID exists
      if (headingId) {
        // Scroll to the heading
        scrollToHeading(headingId, event);
      }
    });
  });

  // Handle initial page load with hash in URL
  if (window.location.hash) {
    // Remove the # symbol from the hash
    const initialHeadingId = window.location.hash.substring(1);

    // Use setTimeout to ensure the page has fully loaded
    setTimeout(() => {
      scrollToHeading(initialHeadingId);
    }, 100);
  } else {
    // If no hash, initialize active TOC based on scroll position
    setTimeout(updateTocOnScroll, 100);
  }

  // Add scroll event listener to update TOC while scrolling
  window.addEventListener('scroll', debounce(updateTocOnScroll, 100));
}

// Debounce function to limit scroll event firing
function debounce(func, wait) {
  let timeout;
  return function() {
    const context = this;
    const args = arguments;
    clearTimeout(timeout);
    timeout = setTimeout(() => func.apply(context, args), wait);
  };
}

// Initialize navigation when the DOM is fully loaded
document.addEventListener('DOMContentLoaded', initNavigation);

// Handle back/forward browser navigation
window.addEventListener('popstate', function() {
  if (window.location.hash) {
    const headingId = window.location.hash.substring(1);
    scrollToHeading(headingId);
  }
});
