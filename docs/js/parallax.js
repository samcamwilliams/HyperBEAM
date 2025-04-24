function applyParallaxToPictures() {
  const pictures = document.querySelectorAll("picture.rocks");
  const header = document.querySelector(".custom-homepage-header");
  const heroSection = document.querySelector(".hero-container");

  pictures.forEach((picture, index) => {
    picture.dataset.depth = (index + 1) * 0.15;
  });

  let ticking = false;

  function onScroll() {
    if (!ticking) {
      window.requestAnimationFrame(() => {
        const scrollTop = window.pageYOffset;

        if (header && heroSection) {
          const fadeStart =
            heroSection.offsetTop + heroSection.offsetHeight * 0.8; // starts 40% down the hero
          const fadeEnd =
            heroSection.offsetTop + heroSection.offsetHeight * 0.93; // ends 90% down the hero

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

        pictures.forEach((picture) => {
          const depth = parseFloat(picture.dataset.depth);
          const offset = scrollTop * depth;
          picture.style.transform = `translateY(${offset}px)`;
        });

        ticking = false;
      });
      ticking = true;
    }
  }

  window.addEventListener("scroll", onScroll);
}

document.addEventListener("DOMContentLoaded", applyParallaxToPictures);
