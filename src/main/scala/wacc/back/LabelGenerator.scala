package wacc.back

class LabelGenerator {

    private var count = 0

    def generate(): String = {
        val label = s".L${count}"
        count += 1
        return label
    }

}