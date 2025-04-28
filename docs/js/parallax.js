function applyParallaxToPictures() {
  const pictures = document.querySelectorAll("picture.rocks");
  const header = document.querySelector(".custom-homepage-header");
  const heroSection = document.querySelector(".hero-container");

  // Initialize depth and offset for each picture
  pictures.forEach((picture, index) => {
    picture.dataset.depth = (index + 1) * 0.15; // or 0.01 if you want ultra subtle
    picture.dataset.offset = "0";
  });

  let lastScrollTop = window.pageYOffset;
  let lastTime = performance.now();
  let velocity = 0; // estimate velocity
  const spring = 0.08; // how fast it springs toward the target
  const damping = 0.85; // how much it slows down

  function lerp(start, end, t) {
    return start * (1 - t) + end * t;
  }

  function update(now) {
    const scrollTop = window.pageYOffset;
    const deltaTime = now - lastTime;
    lastTime = now;

    // Estimate scroll speed
    const deltaScroll = scrollTop - lastScrollTop;
    velocity = velocity * damping + deltaScroll * (1 - damping);
    lastScrollTop = scrollTop;

    // Header fade effect
    if (header && heroSection) {
      const fadeStart = heroSection.offsetTop + heroSection.offsetHeight * 0.8;
      const fadeEnd = heroSection.offsetTop + heroSection.offsetHeight * 0.93;

      let headerOpacity;
      if (scrollTop < fadeStart) {
        headerOpacity = 0;
      } else if (scrollTop > fadeEnd) {
        headerOpacity = 1;
      } else {
        headerOpacity = (scrollTop - fadeStart) / (fadeEnd - fadeStart);
      }

      header.style.backgroundColor = `rgba(255, 255, 255, ${headerOpacity})`;
      header.style.filter = `invert(${1 - headerOpacity})`;
    }

    let stillMoving = false;

    pictures.forEach((picture) => {
      const depth = parseFloat(picture.dataset.depth);
      const currentOffset = parseFloat(picture.dataset.offset);
      const targetOffset = scrollTop * depth;

      // Smoothly spring toward the target
      const springStrength =
        spring + Math.min(Math.abs(velocity) * 0.002, 0.02); // dynamic spring
      const newOffset = lerp(currentOffset, targetOffset, springStrength);

      picture.dataset.offset = newOffset.toString();
      picture.style.transform = `translateY(${newOffset}px)`;

      if (Math.abs(newOffset - targetOffset) > 0.5) {
        stillMoving = true;
      }
    });

    if (stillMoving) {
      requestAnimationFrame(update);
    }
  }

  window.addEventListener("scroll", () => {
    requestAnimationFrame(update);
  });

  // Start the first frame
  requestAnimationFrame(update);
}

document.addEventListener("DOMContentLoaded", applyParallaxToPictures);
