/**
 * Base route for all API calls
 */
const BASE_ROUTE = window.location.href;

/**
 * Format an address for display, shortening it if needed
 * @param {string} address - The full address to format
 * @param {boolean} wrap - Whether to wrap the address in parentheses
 * @returns {string} - The formatted address
 */
function formatAddress(address, wrap) {
  if (!address) return "";
  const formattedAddress =
    address.substring(0, 6) +
    "..." +
    address.substring(36, address.length);
  return wrap ? `(${formattedAddress})` : formattedAddress;
}

/**
 * Format a number for display, adding commas and handling decimals
 * @param {number|string} amount - The amount to format
 * @returns {string} - The formatted amount
 */
function formatDisplayAmount(amount) {
  if (amount === null) return "-";
  if (amount.toString().includes(".")) {
    let parts = amount.toString().split(".");
    parts[0] = parts[0].replace(/\B(?=(\d{3})+(?!\d))/g, ",");

    let firstTwoDecimals = parts[1].substring(0, 2);

    if (firstTwoDecimals === "00") {
      parts[1] = parts[1].substring(0, 6);
    } else {
      parts[1] = parts[1].substring(0, 4);
    }

    return parts.join(".");
  } else {
    return amount.toString().replace(/\B(?=(\d{3})+(?!\d))/g, ",");
  }
}

/**
 * Make a GET request to the server
 * @param {string} endpoint - The endpoint to request
 * @returns {Promise<string>} - The response text
 */
async function get(endpoint) {
  return await (await fetch(`${BASE_ROUTE}${endpoint}`)).text();
}

/**
 * Copy text to clipboard and update button text
 * @param {HTMLButtonElement} button - The button that was clicked
 */
function copyToClipboard(button) {
  const text = button.innerHTML;

  navigator.clipboard
    .writeText(button.value)
    .then(() => {
      button.textContent = "Copied!";
      button.disabled = true;

      setTimeout(() => {
        button.textContent = text;
        button.disabled = false;
      }, 2000);
    })
    .catch((err) => console.error("Error copying text: ", err));
}

/**
 * Show a specific tab content and update tab button styles
 * @param {string} tabId - The ID of the tab to show
 */
function showTab(tabId) {
  // Hide all tab content
  document.querySelectorAll(".tab-content").forEach((content) => {
    content.classList.remove("active");
  });
  
  // Deactivate all tab buttons
  document.querySelectorAll(".tab-button").forEach((button) => {
    button.classList.remove("active");
  });
  
  // Show the selected tab content
  document.getElementById(tabId).classList.add("active");
  
  // Activate the matching tab button
  document.querySelector(`.tab-button[data-tab="${tabId}"]`).classList.add("active");
}

/**
 * Initialize tab event listeners
 */
function initTabListeners() {
  document.querySelectorAll(".tab-button").forEach(button => {
    button.addEventListener("click", () => {
      const tabId = button.getAttribute("data-tab");
      showTab(tabId);
    });
  });
}

// Export functions for use in other modules
export {
  BASE_ROUTE,
  formatAddress,
  formatDisplayAmount,
  get,
  copyToClipboard,
  showTab,
  initTabListeners
}; 