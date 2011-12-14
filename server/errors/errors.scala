
package errors

// The idea of this trait is that we now that a BadUser error
// is not *our* fault; ie, PitFail isn't broken
trait BadUser extends RuntimeException

