/**
 * Implements a complex number
 * @param real complex number real part
 * @param imaginary complex number imaginary part
 * author Shen Li
 * */
class Complex(real: Double, imaginary: Double) extends Ordered[Complex]{
    /**
     * Get complex number real part
     * @return complex number real part
     * */
    def getReal: Double = real

    /**
     * Get complex number imaginary part
     * @return complex number imaginary part
     * */
    def getImaginary: Double = imaginary

    /**
     * Get complex number length
     * @return complex number length
     * */
    def getLength: Double = real * real + imaginary * imaginary

    /**
     * Implements '+' operation
     * @param that the object to be added
     * @return the result complex number after '+' operation
     * */
    def +(that: Complex): Complex = 
        new Complex((this getReal) + (that getReal),
                    (this getImaginary) + (that getImaginary))

    /**
     * Implements '-' operation
     * @param that the object to be minus
     * @return the result complex number after '-' operation
     * */
    def -(that: Complex): Complex = 
        new Complex((this getReal) - (that getReal),
                    (this getImaginary) - (that getImaginary))

    /**
     * Implements '*' operation
     * @param factor the multiple factor
     * @return the result complex number after '*' operation
     * */
    def *(factor: Double): Complex = 
        new Complex((this getReal) * factor, (this getImaginary) * factor)

    /**
     * Implements '/' operation
     * @param factor the divide factor
     * @return the result complex number after '/' operation
     * */
    def /(factor: Double): Complex = 
        new Complex((this getReal) / factor, (this getImaginary) / factor)

    /**
     * Compare this object with the specified object for order
     * @param that the object to be compared
     * @return a negative integer, zero, or a positive integer as this
     * object is less than, equal to, or greater than the specified
     * object.
     * */
    def compare(that: Complex): Int = 
        if ((this getLength) < (that getLength))
            -1
        else if ((this getLength) > (that getLength))
            1
        else
            0

    /**
     * Convert complex number to string format
     * @return complex number string format
     * */
    override def toString: String = 
        "" + real + (if (imaginary < 0) "" else "+") + imaginary + "i"
}

/**
 * Implement insertion sort and merge sort
 * */
object Sort{
    /**
     * Insertion sort
     * @param x the object to be inserted
     * @param l the sorted list in increasing order
     * @return the list containing x and elements of l in increasing order
     * */
    def insert_sorted[A <: Ordered[A]](x: A, l: List[A]): List[A] = l match {
        case List() => List(x)
        case y::ys => if ((x compare y) <= 0) x::l else y::insert_sorted(x, ys)
    }

	/**
     * Implements merge two lists in increasing order
     * @param l1 the first list to be merged
     * @param l2 the second list to be merged
     * @return the list containing the elements of l1 and l2 in
     * increasing order
     * */
    def merge[A <: Ordered[A]](l1: List[A], l2: List[A]): List[A] = 
		if (l1.isEmpty)
			l2
		else if (l2.isEmpty)
			l1
		else if ((l1.head compare l2.head) <= 0)
			l1.head::merge(l1.tail, l2)
		else
			l2.head::merge(l1, l2.tail)

    /**
     * Merge sort
     * @param l the list to be sorted
     * @return the sorted list
     * */
    def mergesort[A <: Ordered[A]](l: List[A]): List[A] = {
        val mid: Int = l.length/2
        if (mid == 0)
            l
        else
            merge(mergesort(l take mid), mergesort(l drop mid))
    }
}

/**
 * Implements a generic sorted list
 * @param l the list to be sorted
 * */
class SortedList[E <: Ordered[E]](l: List[E]){
    /**
     * Get the sorted list
     * @return the sorted list
     * */
    def get: List[E] = Sort mergesort l

    /**
     * Get the first element of object
     * @return the first element
     * */
    def head: E = (this get) match{
        case Nil => sys.error("Nil.head")
        case x::xs => x
    }

    /**
     * Get all elements of object except the first element
     * @return a SortedList object containingall elements of object
     * except the first element
     * */
    def tail: SortedList[E] = (this get) match {
        case Nil => sys.error("Nil.tail")
        case x::xs => new SortedList[E](xs)
    }

    /**
     * Insert element into the sorted list
     * @param x the element to be inserted
     * @return a SortedList object containing x and the elements of
     * this object
     * */
    def insert(x: E): SortedList[E] = 
        new SortedList[E](Sort insert_sorted(x, this.get))

    /**
     * Append two SortedList objects
     * @param sl the SortedList object to be appended
     * @return a SortedList object containing all the elements of this
     * object as well as all elements of sl
     * */
    def append(sl: SortedList[E]): SortedList[E] = 
        new SortedList[E](Sort merge(this.get, sl.get))

    /**
     * Map the function to each element of this object
     * @param f the map function
     * @return a SortedList object containing values resulting from 
     * applying function to each element of this object
     * */
    def map[B <: Ordered[B]](f: E => B): SortedList[B] = {
        def helper(ll: List[E]): List[B] = ll match {
            case List() => List()
            case x::xs => f(x)::helper(xs)
        }
        new SortedList[B](Sort mergesort (helper(l)))
    }

    /**
     * Flatmap the function to each element of this object
     * @param f the flatmap function
     * @return a SortedList object containing all values in all SortedList's
     * resulting from applying function to each element of this object
     * */
    def flatMap[B <: Ordered[B]](f: E => SortedList[B]): SortedList[B] = {
        def helper(ll: List[E]): List[B] = ll match {
            case List() => List()
            case x::xs => (f(x) get):::helper(xs)
        }
        new SortedList[B](Sort mergesort (helper(l)))
    }

    /**
     * Filter each element of this object
     * @param p the filter function
     * @return a SortedList object containing elements of this object after
     * filtered
     * */
    def filter(p: E => Boolean): SortedList[E] = {
        def helper(ll: List[E]): List[E] = ll match {
            case List() => List()
            case x::xs => if (p(x)) x::helper(xs) else helper(xs)
        }
        new SortedList[E](helper(this get))
    }

    /**
     * Applies function to each element of this object
     * @param f the foreach function
     * */
    def foreach(f: E => Unit) {
        def helper(ll: List[E]) {
            ll match {
                case List() => ()
                case x::xs => f(x); helper(xs)
            }
        }
        helper(this get)
    }

    /**
     * Convert SortedList object to string format
     * @return SortedList object string format
     * */
    override def toString: String = "SortedList(" + {
        def helper(li: List[E]): String = li match {
            case Nil => ""
            case List(x) => x toString
            case x::xs => (x toString) + ", " + helper(xs)
        }
        helper(l)
    } + ")"
}

/**
 * Test SortedList class
 * */
object Test{
    /**
     * @param args the command line arguments
     * */
    def main(args: Array[String]){
        val l1: List[Complex] = List(   new Complex(1.8, -4.2),
                                        new Complex(4.1, 0.13),
                                        new Complex(-1.3, 2.1),
                                        new Complex(7.9, 8.21),
                                        new Complex(0.34, -0.95),
                                        new Complex(2.24, 3.301),
                                        new Complex(-0.298, -6.8))
        val l2: List[Complex] = List(   new Complex(2.8, 9.2),
                                        new Complex(-4.1, -0.2),
                                        new Complex(5.30, -0.1),
                                        new Complex(1.901, 0.29),
                                        new Complex(-10.3, 0.37),
                                        new Complex(-2.23, 1.3))
        val c: Complex = new Complex(2.7, 3.1)
        val l3: List[Complex] = for (x <- l1) yield (x + c)
        val l4: List[Complex] = for (x <- l1; y <- l2) yield (x + y * 5)
        val l5: List[Complex] = for (x <- l1 if (x getReal) > 0) yield x

        val sorted_l1: SortedList[Complex] = new SortedList[Complex](l1)
        val sorted_l2: SortedList[Complex] = new SortedList[Complex](l2)
        val sorted_l3: SortedList[Complex] = new SortedList[Complex](l3)
        val sorted_l4: SortedList[Complex] = new SortedList[Complex](l4)
        val sorted_l5: SortedList[Complex] = new SortedList[Complex](l5)

        //Output sorted_l1
        println("\nSortedList[Complex] object one:\n" + (sorted_l1 toString))
        //Output sorted_l2
        println("\nSortedList[Complex] object two:\n" + (sorted_l2 toString))
        //Output complex number c
        println("\nComplex number:\n" + (c toString))
        //Output 'get' method result
        println("\nSortedList[Complex] object one 'get' method result:\n"
                + (sorted_l1 get))
        //Output 'head' method result
        println("\nSortedList[Complex] object one 'head' method result:\n"
                + ((sorted_l1 head) toString))
        //Output 'tail' method result
        println("\nSortedList[Complex] object one 'tail' method result:\n"
                + ((sorted_l1 tail) toString))
        //Output 'insert' method result
        println("\nSortedList[Complex] object one 'insert' method result:"
                + "(insert complex number c)\n"
                + ((sorted_l1 insert c) toString))
        //Output 'append' method result
        println("\nSortedList[Complex] object 'append' method result:"
                + "(append the two SortedList[Complex] objects)\n"
                + ((sorted_l1 append sorted_l2) toString))
        //Output 'map' method result
        println("\nSortedList[Complex] object one 'map' method result:"
                + "(each element plus complex number c)\n"
                + (sorted_l1 map (x => x + c) toString))
        //Output for-comprehension result
        println("\n'map' method implemented by for-comprehension:\n"
                + (sorted_l3 get))
        //Output 'flatmap' method result
        println("\nSortedList[Complex] object one 'flatmap' method result:"
                + "(each element add the result of mapping to SortedList[Complex] object two whose each element multiple by 5)\n"
                + ((sorted_l1 flatMap (x => sorted_l2 map (y => x + y * 5))) toString))
        //Output for-comprehension result
        println("\n'flatmap' method implemented by for-comprehension:\n"
                + (sorted_l4 get))
        //Output 'filter' method result
        println("\nSortedList[Complex] object one 'filter' method result:"
                + "(filter element whose real part greater than 0)\n"
                + ((sorted_l1 filter (x => (x getReal) > 0)) toString))
        //Output for-comprehension result
        println("\n'filter' method implemented by for-comprehension:\n"
                + (sorted_l5 get))
        //Output 'foreach' method result
        println("\nSortedList[Complex] object one 'foreach' method result:"
                + "(output each element)\n")
        sorted_l1 foreach (x => println(x toString))
        //Output for-comprehension result
        println("\n'foreach' method implemented by for-comprehension:\n")
        for (x <- (sorted_l1 get))  println(x toString)
    }
}
