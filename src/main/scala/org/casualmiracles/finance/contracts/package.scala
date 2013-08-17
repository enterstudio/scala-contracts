package org.casualmiracles.finance

import org.joda.time.LocalDate

package object contracts {
  type RV[A] = Stream[A]
  type TimeStep = Int
  type CalendarTime = LocalDate
}
