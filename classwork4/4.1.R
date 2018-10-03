predict.plm <- function(obj, dt) {
  # Проверим, есть ли нужные нам компоненты в объекте
  stopifnot("a" %in% names(obj), "y0" %in% names(obj))
  a <- obj$a
  y0 <- obj$y0
  # Проверка входных данных
  stopifnot(is.numeric(a),length(a)==1)
  stopifnot(is.numeric(y0),length(y0)==1)
  stopifnot(is.numeric(dt))
  return(y0*dt^a) # Вычислим и выйдем
}