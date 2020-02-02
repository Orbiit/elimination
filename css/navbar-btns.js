for (const elem of document.querySelectorAll('.header-btn')) {
  elem.addEventListener('click', e => {
    if (document.querySelector('.open')) {
      document.querySelector('.open').classList.remove('open')
    }
    elem.parentNode.classList.add('open')
  })
}
document.addEventListener('click', e => {
  if (e.target.closest('.header-window-wrapper')) return
  if (document.querySelector('.open')) {
    document.querySelector('.open').classList.remove('open')
  }
})
