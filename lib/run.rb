require 'lib/scheduler.rb'

def update_progress(i)
  print "\r\e[0K#{i}"
  $stdout.flush
end

def finish_progress
  print "\r\e[0K"
  $stdout.flush
end

puts "Reading classes...\n\n" # read classes

classes = []
progress = 1

CSV.foreach "classes.csv" do |row|
  classes.push(ChemClass.new(row[0], row[1], row[2..-1]))
  update_progress progress
  progress = progress.next
end
finish_progress

#classes.each do |c|
#  puts c
#end

puts "\nDone reading classes.\n\n"

puts "Reading students...\n\n" # read students & schedules

students = []
progress = 1

CSV.foreach "tas.csv" do |row|
  students.push(Student.new(row[0], row[1].to_f, row[2..-1]))
  update_progress progress
  progress = progress.next
end
finish_progress

#students.each do |s|
#  puts s
#end

puts "\nDone reading students.\n\n"



puts "Determining schedules..."
s = Scheduler.new classes, students
puts "Done determining schedules.\n\n"

max_rating = s.scenarios[0].rating

optimum_schedules = s.scenarios.find_all { |s| s.rating == max_rating }
#o = optimum_schedules.clone
#i = 0

#while (s = o.slice!(0)) != nil
#  optimum_schedules.slice!(i) if o.include?(s)
#  i = i.next
#end

optimum_schedules.each do |s|
  puts "schedule: #{s.rating}\n"
  s.pairings.each do |p|
    puts 'pairing'
    puts p.student
    puts p.chem_class
  end
  puts "\n\n"
end
