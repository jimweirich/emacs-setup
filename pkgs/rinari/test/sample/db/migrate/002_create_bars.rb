class CreateBars < ActiveRecord::Migration
  def self.up
    create_table :bars do |t|
      # t.column :name, :string
    end
  end

  def self.down
    drop_table :bars
  end
end
