document.addEventListener('DOMContentLoaded', function() {
    // Get all navigation links
    const navLinks = document.querySelectorAll('.md-nav__link');
    
    // Add click event listener to each link
    navLinks.forEach(link => {
        link.addEventListener('click', function(e) {
            // Only handle internal links
            if (this.href && this.href.startsWith(window.location.origin)) {
                e.preventDefault();
                
                // Get the target URL
                const targetUrl = this.href;
                
                // Use History API to update URL without reload
                history.pushState({}, '', targetUrl);
                
                // Fetch the new content
                fetch(targetUrl)
                    .then(response => response.text())
                    .then(html => {
                        // Create a temporary container
                        const temp = document.createElement('div');
                        temp.innerHTML = html;
                        
                        // Update the main content
                        const newContent = temp.querySelector('.md-content__inner');
                        const currentContent = document.querySelector('.md-content__inner');
                        if (newContent && currentContent) {
                            currentContent.innerHTML = newContent.innerHTML;
                        }
                        
                        // Update the active state in navigation
                        navLinks.forEach(navLink => {
                            navLink.classList.remove('md-nav__link--active');
                        });
                        this.classList.add('md-nav__link--active');
                        
                        // Scroll to top
                        window.scrollTo(0, 0);
                        
                        // Update page title
                        const newTitle = temp.querySelector('title');
                        if (newTitle) {
                            document.title = newTitle.textContent;
                        }
                    })
                    .catch(error => {
                        console.error('Error during navigation:', error);
                        // Fallback to regular navigation if fetch fails
                        window.location.href = targetUrl;
                    });
            }
        });
    });
    
    // Handle browser back/forward buttons
    window.addEventListener('popstate', function() {
        window.location.reload();
    });
}); 