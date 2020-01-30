let tabFocus = false
document.addEventListener('keydown', e => {
  if (e.key === 'Tab') {
    document.body.classList.add('focus-visible')
    tabFocus = true
  } else if (e.target.tagName === 'SPAN' && e.key === 'Enter') {
    e.target.click()
  }
})
document.addEventListener('keyup', e => {
  if (e.key === 'Tab') {
    tabFocus = false
  }
})
document.addEventListener('focusin', e => {
  if (!tabFocus) {
    document.body.classList.remove('focus-visible')
  }
})
