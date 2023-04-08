package io.github.andjsrk.v8.parsing

class ResettableState<T>(private val initialValue: T) {
    var value = initialValue
    fun reset() {
        value = initialValue
    }
}
