document.addEventListener('DOMContentLoaded', () => {
  // Select all links within the main navigation tabs
  const tabLinks = document.querySelectorAll('nav.md-tabs .md-tabs__list .md-tabs__item a');

  tabLinks.forEach(link => {
    link.addEventListener('click', function(event) {
      // Basic check if it's an internal link (avoids issues if external links are ever added)
      // You might refine this check based on your site structure if needed.
      if (link.hostname === window.location.hostname || !link.hostname.length) {

        console.log('Tab clicked, forcing full reload for:', link.href); // Optional: for debugging

        // Prevent the default navigation behavior AND stop the event from bubbling up
        // to the theme's instant navigation handler.
        event.preventDefault();
        event.stopPropagation();

        // Force a full page load
        window.location.href = link.href;
      }
    }, true); // Use capture phase to try and catch the event before the theme's handler
  });
}); 