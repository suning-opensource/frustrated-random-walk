//@Author Enzhi Li
//This file is created by Enzhi Li at Suning R & D Center, Palo Alto, CA, in 2019.
//If you want to use or modify this program, please get permission from Enzhi Li first. 
//All rights reserved.

package frustrated.random.walk

class Vertex
{
    var color: String = "white"
    var id: String = ""
    var label: String = ""
    var index: Int = -1
    var lowlink: Int = -1
    var onStack: Boolean = false
    var parent :Vertex = null

    def this(id: String, label: String) = 
    {
        this()
        this.id = id
        this.label = label
    }

    def this(that: Vertex) = 
    {
        this(that.id, that.label)
    }

    def < (that: Vertex): Boolean = 
    {
        if (this.label == that.label)
        {
            return this.id < that.id
        }
        else
        {
            return this.label < that.label
        }
    }

    override def toString: String = 
    {
        return "[id:" + this.id.toString + ", label:" + this.label + ", color:" + this.color + "]"
    }

    override def equals(other: Any) = other match 
    {
        case that: Vertex => this.id == that.id && this.label == that.label
        case _ => false
    }

    def > (that: Vertex): Boolean = 
    {
        if (! (this < that) && ! (this.equals(that)))
        {
            return true
        }
        return false
    }

    override def hashCode: Int = 
    {
        var result: Int = this.id.hashCode
        result += 31 * result + this.label.hashCode
        return result
    }
}
