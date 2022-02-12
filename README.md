# dequeue

Catenable Dequeue's. All atomic operations (besides `fromList`, which is `O(n)`) are
guaranteed amortized `O(1)`, _even_ when used persistently. This includes `cons`, `snoc`, `uncons`,
`unsnoc`, `(<>)`, `reverse`, and `length`.
