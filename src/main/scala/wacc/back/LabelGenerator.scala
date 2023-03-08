package wacc
package back

class LabelGenerator {
    /*generates labels for ifs and whiles */
    private var count = 0

    def generate(): String = {
        val label = s".L${count}"
        count += 1
        return label
    }

}