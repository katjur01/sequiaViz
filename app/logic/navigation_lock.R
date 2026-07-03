# Navigation Locking Helpers
# Functions for managing tab locking to control user navigation flow

box::use(
  shiny[...],
  htmltools[tags, HTML]
)

#' Lock navigation tabs except specified one
#'
#' @param session Shiny session object
#' @param allowed_value Tab value that should remain accessible (e.g., ns("upload_data"))
#' @export
lock_navigation <- function(session, allowed_value) {
  session$sendCustomMessage("lockMenuExcept", list(
    lock = TRUE,
    allowValue = allowed_value
  ))
}

#' Unlock all navigation tabs
#'
#' @param session Shiny session object
#' @export
unlock_navigation <- function(session) {
  session$sendCustomMessage("lockMenuExcept", list(
    lock = FALSE,
    allowValue = NULL
  ))
}

#' Enable only a given set of top-navbar tabs, disable the rest with an optional tooltip
#'
#' Only the top navbar menu is affected, never the tabs inside modules. Disabled
#' tabs stay visible but their click is blocked by the click guard, while the anchor
#' still receives the mouse so its title tooltip and not-allowed cursor show on hover.
#'
#' @param session Shiny session object
#' @param enabled_values Character vector of tab values that should stay clickable
#' @param tooltips Named list mapping a disabled tab value to its tooltip text
#' @export
set_enabled_tabs <- function(session, enabled_values, tooltips = list()) {
  session$sendCustomMessage("setEnabledTabs", list(
    enabled = as.list(enabled_values),
    tooltips = tooltips
  ))
}

#' Get CSS styles for locked and disabled tabs
#'
#' @return HTML style tag with CSS
#' @export
get_navigation_lock_css <- function() {
  tags$style(HTML("
    .app-tab-locked {
      pointer-events: none;
      opacity: 0.5;
      cursor: not-allowed !important;
    }
    /* Data-dependent navbar tab with no data yet: visible, hoverable, but click is blocked */
    .app-tab-disabled {
      opacity: 0.4;
      cursor: not-allowed;
    }
    /* Instant custom tooltip for disabled navbar tabs */
    .app-tab-disabled {
      position: relative;
    }
    .app-tab-disabled[data-tip]:hover::after {
      content: attr(data-tip);
      position: absolute;
      top: 100%;
      left: 50%;
      transform: translateX(-50%);
      margin-top: 4px;
      white-space: nowrap;
      background: #333e48;
      color: #fff;
      font-size: 13px;
      font-weight: normal;
      padding: 6px 10px;
      border-radius: 4px;
      z-index: 1000;
      pointer-events: none;
    }
  "))
}

#' Get JavaScript code for navigation locking mechanism
#'
#' @return HTML script tag with JavaScript handlers
#' @export
get_navigation_lock_js <- function() {
  tags$script(HTML("
    (function(){
      var menuLocked = true;
      var allowedValue = null;

      Shiny.addCustomMessageHandler('lockMenuExcept', function(msg){
        // msg: { lock: bool, allowValue: 'ns(tabName)' }
        menuLocked = !!msg.lock;
        allowedValue = msg.allowValue;

        var anchors = document.querySelectorAll('a.nav-link[data-value], a.dropdown-item[data-value]');
        anchors.forEach(function(a){
          var v = a.getAttribute('data-value');
          if (!v) return;
          if (menuLocked && v !== allowedValue) {
            a.classList.add('app-tab-locked');
            a.setAttribute('aria-disabled', 'true');
            a.setAttribute('tabindex', '-1');
          } else {
            a.classList.remove('app-tab-locked');
            a.removeAttribute('aria-disabled');
            a.removeAttribute('tabindex');
          }
        });
      });

      // Enable a set of top-navbar tabs, disable the rest and give them a tooltip.
      // Scoped to the top navbar only so it never touches tabs inside modules.
      Shiny.addCustomMessageHandler('setEnabledTabs', function(msg){
        var enabled = msg.enabled || [];
        if (typeof enabled === 'string') enabled = [enabled];
        var tips = msg.tooltips || {};
        var anchors = document.querySelectorAll('.main-header .navbar-nav.sidebar-menu a.nav-link[data-value]');
        anchors.forEach(function(a){
          var v = a.getAttribute('data-value');
          if (!v) return;
          if (enabled.indexOf(v) !== -1) {
            a.classList.remove('app-tab-disabled');
            a.removeAttribute('aria-disabled');
            a.removeAttribute('data-tip');
          } else {
            a.classList.add('app-tab-disabled');
            a.setAttribute('aria-disabled', 'true');
            var t = tips[v];
            if (t) { a.setAttribute('data-tip', t); } else { a.removeAttribute('data-tip'); }
          }
        });
      });

      // Hard click guard: blocks startup-locked tabs and data-disabled tabs
      document.addEventListener('click', function(e){
        var t = e.target.closest('a.nav-link[data-value], a.dropdown-item[data-value]');
        if (!t) return;
        if (menuLocked) {
          var v = t.getAttribute('data-value');
          if (v && v !== allowedValue) {
            e.preventDefault();
            e.stopImmediatePropagation();
            return false;
          }
        }
        if (t.classList.contains('app-tab-disabled')) {
          e.preventDefault();
          e.stopImmediatePropagation();
          return false;
        }
      }, true); // capture phase for safety
    })();
  "))
}