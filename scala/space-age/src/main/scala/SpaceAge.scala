

object SpaceAge {
    def onEarth( age:Double) : Double = {
        age / 60 / 60 / 24 / 365.25
    }

    def onMercury ( age:Double) : Double = onEarth(age) /  0.2408467 
    def onVenus ( age:Double) : Double = onEarth(age) / 0.61519726 
    def onMars ( age:Double) : Double = onEarth(age) /  1.8808158
    def onJupiter ( age:Double) : Double = onEarth(age) / 11.862615
    def onSaturn ( age:Double) : Double = onEarth(age) / 29.447498 
    def onUranus ( age:Double) : Double = onEarth(age) / 84.016846 
    def onNeptune ( age:Double) : Double = onEarth(age) / 164.79132

}