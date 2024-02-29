HOURS_IN_DAY = 24
MINUTES_IN_HOUR = 60
SECONDS_IN_MINUTE = 60


class Time:
    """ Represents a time of day."""
    def __init__(self, hours, minutes, seconds):
        """ Initialises a Time object with integers 'hours', 'minutes' and
        'seconds.
        >>> t = Time(18, 30, 0)
        """
        while seconds < 0:
            seconds += SECONDS_IN_MINUTE
            minutes -= 1
        while minutes < 0:
            minutes += MINUTES_IN_HOUR
            hours -= 1
        while hours < 0:
            hours += HOURS_IN_DAY
        while seconds >= SECONDS_IN_MINUTE:
            seconds -= SECONDS_IN_MINUTE
            minutes += 1
        while minutes >= MINUTES_IN_HOUR:
            minutes -= MINUTES_IN_HOUR
            hours += 1

        self.hours = hours
        self.minutes = minutes
        self.seconds = seconds % SECONDS_IN_MINUTE

    def __repr__(self):
        """ Returns the string representation of a Time object.
        >>> print( Time(8,5,30) )
        08:05:30
        """
        hour = str(self.hours)
        if self.hours < 10:
            hour = "0" + str(self.hours)
        minutes = str(self.minutes)
        if self.minutes < 10:
            minutes = "0" + str(self.minutes)
        seconds = str(self.seconds)
        if self.seconds < 10:
            seconds = "0" + str(self.seconds)
        return f"{hour}:{minutes}:{seconds}"

    def get_hours(self):
        """ Returns the hours of the Time object.
        >>> Time(23,0,0).get_hours()
        23
        """
        return self.hours

    def get_minutes(self):
        """ Returns the minutes of the Time object.
        >>> Time(0,59,0).get_minutes()
        59
        """
        return self.minutes

    def get_seconds(self):
        """ Returns the seconds of the Time object.
        >>> Time(0,0,59).get_seconds()
        59
        """
        return self.seconds

    def set_time(self, hours, minutes, seconds):
        """ Sets the time of the Time object to 'hours', 'minutes',
        and 'seconds' making sure the values are in valid range:
          hours:   [0, HOURS_IN_DAY)
          minutes: [0, MINUTES_IN_HOUR)
          seconds: [0, SECONDS_IN_MINUTE)
        >>> time = Time(0, 0, 0)
        >>> time.set_time(0, 0, 90)
        >>> print(time)
        00:01:30
        >>> time.set_time(0, 0, 3600)
        >>> print(time)
        01:00:00
        >>> time.set_time(0, 0, -1)
        >>> print(time)
        23:59:59
        >>> time.set_time(10, -121, 0)
        >>> print(time)
        07:59:00
        >>> time.set_time(-50, 0, 0)
        >>> print(time)
        22:00:00
        >>> print(Time(10, -120, -150)) # __init__() test
        07:57:30
        """
        while seconds < 0:
            seconds += SECONDS_IN_MINUTE
            minutes -= 1
        while minutes < 0:
            minutes += MINUTES_IN_HOUR
            hours -= 1
        while hours < 0:
            hours += HOURS_IN_DAY
        while seconds >= SECONDS_IN_MINUTE:
            seconds -= SECONDS_IN_MINUTE
            minutes += 1
        while minutes >= MINUTES_IN_HOUR:
            minutes -= MINUTES_IN_HOUR
            hours += 1

        self.hours = hours
        self.minutes = minutes
        self.seconds = seconds % SECONDS_IN_MINUTE
