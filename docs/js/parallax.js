function applyParallaxToPictures() {
  const pictures = document.querySelectorAll("picture.rocks");
  const header = document.querySelector(".custom-homepage-header");
  const heroSection = document.querySelector(".hero-container");

  pictures.forEach((picture, index) => {
    picture.dataset.depth = (index + 1) * 0.15;
    picture.dataset.offset = "0";
  });

  let needsUpdate = false;

  function lerp(start, end, t) {
    return start * (1 - t) + end * t;
  }

  function update() {
    needsUpdate = false;
    const scrollTop = window.pageYOffset;

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
      header.style.backgroundColor = `rgba(255, 255, 255, ${headerOpacity})`;
      header.style.filter = `invert(${1 - headerOpacity})`;
    }

    let stillMoving = false;

    pictures.forEach((picture) => {
      const depth = parseFloat(picture.dataset.depth);
      const currentOffset = parseFloat(picture.dataset.offset);
      const targetOffset = scrollTop * depth;

      const newOffset = lerp(currentOffset, targetOffset, 0.06);

      if (Math.abs(newOffset - targetOffset) > 0.5) {
        stillMoving = true;
        picture.dataset.offset = newOffset.toString();
        picture.style.transform = `translateY(${newOffset}px)`;
      } else {
        // Snap exactly when close enough
        picture.dataset.offset = targetOffset.toString();
        picture.style.transform = `translateY(${targetOffset}px)`;
      }
    });

    if (stillMoving || needsUpdate) {
      requestAnimationFrame(update);
    }
  }

  window.addEventListener("scroll", () => {
    needsUpdate = true;
    requestAnimationFrame(update);
  });

  requestAnimationFrame(update);
}

document.addEventListener("DOMContentLoaded", applyParallaxToPictures);
