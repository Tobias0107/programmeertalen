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
        self.hours = hours
        self.minutes = minutes
        self.seconds = seconds

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
