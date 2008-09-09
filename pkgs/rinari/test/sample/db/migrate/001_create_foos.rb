class CreateFoos < ActiveRecord::Migration
  def self.up
    create_table :foos do |t|
      # t.column :name, :string
    end
  end

  def self.down
    drop_table :foos
  end
end
