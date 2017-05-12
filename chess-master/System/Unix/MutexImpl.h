#ifndef SYSTEM_MUTEXIMPL_H
#define SYSTEM_MUTEXIMPL_H

////////////////////////////////////////////////////////////
// Headers
////////////////////////////////////////////////////////////
#include <System/NonCopyable.h>
#include <pthread.h>


namespace System
{
namespace priv
{
////////////////////////////////////////////////////////////
/// \brief Unix implementation of mutexes
////////////////////////////////////////////////////////////
class MutexImpl : NonCopyable
{
public :

    ////////////////////////////////////////////////////////////
    /// \brief Default constructor
    ///
    ////////////////////////////////////////////////////////////
    MutexImpl();

    ////////////////////////////////////////////////////////////
    /// \brief Destructor
    ///
    ////////////////////////////////////////////////////////////
    ~MutexImpl();

    ////////////////////////////////////////////////////////////
    /// \brief Lock the mutex
    ///
    ////////////////////////////////////////////////////////////
    void Lock();

    ////////////////////////////////////////////////////////////
    /// \brief Unlock the mutex
    ///
    ////////////////////////////////////////////////////////////
    void Unlock();

private :

    ////////////////////////////////////////////////////////////
    // Member data
    ////////////////////////////////////////////////////////////
    pthread_mutex_t myMutex; ///< pthread handle of the mutex
};

} // namespace priv

} // namespace System


#endif // SYSTEM_MUTEXIMPL_H
