//@Author Enzhi Li
//This file is created by Enzhi Li at Suning R & D Center, Palo Alto, CA, in 2019.
//If you want to use or modify this program, please get permission from Enzhi Li first. 
//All rights reserved.

package frustrated.random.walk 

class Edge(var left: Vertex, var right: Vertex, var weight: Double, var id: String, var label: String)
{
    override def toString: String = 
    {
        return "(" + left.toString + ") ==> (" + right.toString + "); edge weight = " + this.weight.toString + "; edge id = " + this.id + "; edge label = " + this.label
    }
}
