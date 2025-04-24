function applyParallaxToPictures() {
  const pictures = document.querySelectorAll("picture.rocks");

  pictures.forEach((picture, index) => {
    picture.dataset.depth = (index + 1) * 0.1;
  });

  let ticking = false;

  function onScroll() {
    if (!ticking) {
      window.requestAnimationFrame(() => {
        const scrollTop = window.pageYOffset;
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
