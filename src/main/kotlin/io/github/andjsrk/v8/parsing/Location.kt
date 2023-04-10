package io.github.andjsrk.v8.parsing

data class Location(var start: Int = 0, var end: Int = 0) {
    val length get() = end - start
    val isValid get() = start in 0..end
    companion object {
        val INVALID = Location(-1, 0)
        inline fun since(start: Int, length: Int) =
            Location(start, start + length)
        inline fun untilNext(start: Int) =
            Location.since(start, 1)
    }
}
