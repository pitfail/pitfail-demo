
package model

package object serialization {
//

import java.io.{
    ObjectOutputStream,
    ObjectInputStream,
    ByteArrayOutputStream,
    ByteArrayInputStream
}

def serialize(o: Serializable): Array[Byte] = {
    val bs = new ByteArrayOutputStream(4096)
    val os = new ObjectOutputStream(bs)
    os.writeObject(o)
    os.close()
    bs.toByteArray
}

def deserialize[A](bytes: Array[Byte]) = {
    val bs = new ByteArrayInputStream(bytes)
    val os = new ObjectInputStream(bs)
    os.readObject().asInstanceOf[A]
}

//
}

