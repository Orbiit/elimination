for (const elem of document.querySelectorAll('.auth-btn')) {
  elem.addEventListener('click', e => {
    if (document.querySelector('.open')) {
      document.querySelector('.open').classList.remove('open')
    }
    elem.parentNode.classList.add('open')
  })
}
document.addEventListener('click', e => {
  if (e.target.closest('.auth-wrapper')) return
  if (document.querySelector('.open')) {
    document.querySelector('.open').classList.remove('open')
  }
})
