class WeekTime
  attr_reader :day, :hour, :minute

  def initialize(d, h, m = nil)
    if @day.is_a? WeekDay
      @day = d
    else
      @day = WeekDay.new d
    end

    @hour = h
    @minute = m

    if @hour.is_a? String
      init_time_from_string @hour
    end

    @hour = 0 if @hour == nil
    @minute = 0 if @minute == nil
  end

  def ==(wt)
    self.to_mpsm == wt.to_mpsm
  end

  def <(wt)
    self.to_mpsm < wt.to_mpsm
  end

  def <=(wt)
    self.to_mpsm <= wt.to_mpsm
  end
  
  def >(wt)
    self.to_mpsm > wt.to_mpsm
  end
  
  def >=(wt)
    self.to_mpsm >= wt.to_mpsm
  end

  def to_mpsm
    @day.to_mpsm + @hour * 60 + @minute
  end

  def to_s
    "#{@day.to_s} #{@hour.to_s}:#{@minute.to_s}"
  end

  private

  def init_time_from_string s
    a_or_p = s[-1..-1]

    hours_and_minutes = s[0..-2]
    hours_and_minutes = hours_and_minutes.split ':'

    @hour = hours_and_minutes[0].to_i
    @minute = hours_and_minutes[1].to_i

    if a_or_p == 'p'
      @hour += 12 if @hour != 12
    end
  end
end