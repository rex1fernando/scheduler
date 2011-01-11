require 'rubygems'
require 'csv'
require 'gearman'
Dir[File.join(File.dirname(__FILE__), 'scheduler/*.rb')].sort.each { |lib| require lib }

class Scheduler
  attr_accessor :scenarios

  def initialize(classes, students, m = true)
    @classes = classes
    @students = students
    @is_master = m

    if @is_master
      @client = Gearman::Client.new('127.0.0.1:4730')
   
      starting_scenario = Scenario.new @classes, @students
      @scenarios = determine_scenarios starting_scenario
 
      return @scenarios.sort! { |s1,s2| s1.rating <=> s2.rating }.reverse!
    else
      @worker = Gearman::Worker.new("127.0.0.1:4730")
      @worker.add_ability('determine_scenarios') do |data, job|
        print 'received job. Working...'
        s = determine_scenarios(data)
        puts 'Done.'
        Marshal.dump(s)
      end

      loop { @worker.work }
    end
  end

  private

  def determine_scenarios(scenario)
    scenario = Marshal.load(scenario) if scenario.is_a? String

    taskset = Gearman::TaskSet.new(@client) if @is_master

    scenarios = []

    scenario.pairings.each do |pairing|
      if !pairing.finalized?
        new_pairings = scenario.pairings.clone
        new_pairing = pairing.finalize
        new_pairings[new_pairings.index(pairing)] = new_pairing

        s = Scenario.new scenario.classes, scenario.students, new_pairings        

        if s.pairings.map {|p| p.finalized? }.include? false
          if (@is_master)
            task = Gearman::Task.new('determine_scenarios', Marshal.dump(s))
            task.on_complete{ |d| print '.'; scenarios.concat(Marshal.load(d))}
            task.on_fail { puts "Worker failed."; exit }
            task.on_exception { |e| puts "exception:#{e}"; exit }
            task.on_retry { 2 }
            taskset.add_task(task)            
          else
            scenarios.concat(determine_scenarios(s))
          end
        else
          scenarios.push s
        end
      end
    end

    taskset.wait(100000) if @is_master

    scenarios
  end
end
