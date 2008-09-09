;;; *****************************************************************
;;; Ruby on Rails Code Templates
;;; Date:    17/Mar/06
;;; Author:  Jim Weirich
;;; Purpose: This file provides several templates for Ruby on Railsprogramming.
;;; *****************************************************************

(require 'jw-templates)
(require 'ruby-templates)
(setq tempo-interactive t)

;;; ==================================================================
;;; Helper functions.  This functions call out to rails inflectors, so
;;; rails needs to be installed and available.
;;; ==================================================================

(defun rails-inflect (how word)
  (shell-command-to-string 
   (concat "ruby -rubygems -e 'require \"active_support/inflector\"; print Inflector."
           how
           "(\""
           word
           "\")'")) )

(defun rails-pluralize (word)
  (rails-inflect "pluralize" word))

(defun rails-singularize (word)
  (rails-inflect "singularize" word))

(defun rails-camalize (word)
  (rails-inflect "camilize" word))

(defun rails-underscore (word)
  (rails-inflect "underscore" word))

(defun rails-tableize (class-name)
  (rails-inflect "tableize" class-name))

(defun rails-classify (table-name)
  (rails-inflect "classify" table-name))

(defun rails-humanize (lower-case-ane-underscore-word)
  (rails-inflect "humanize" lower-case-ane-underscore-word))

(defun rails-demodulize (class-name-in-module)
  (rails-inflect "demodulize" class-name-in-module))


;;; ==================================================================
;;; Templates
;;; ==================================================================

;;; Controller Snippets

(jw-template
 "render-component-action"
 '(
   (p "Action: " action t)
   "render_component(:action => '" (s action) "')" > p n ))
 
(jw-template
 "render-component-controller"
 '(
   (p "Controller: " controller t)
   "render_component(:controller => '" (s controller) "')" > p n ))

(jw-template
 "render-component-controller-action"
 '(
   (p "Controller: " controller t)
   (p "Action: " action t) 
   "render_component(:controller => '" (s controller) "', :action => '" (s action) "')" > p n ))

(jw-template
 "redirect-to-action"
 '(
   (p "Action: " action t) 
   "redirect_to(:action => '" (s action) "')" > p n))

(jw-template
 "redirect-to-action-id"
 '(
   (p "Action: " action t)
   (p "ID: " id t)
   "redirect_to(:action => '" (s action) "', :id => " (s id) ")" > p n))

(jw-template
 "redirect-to-controller"
 '(
   (p "Controller: " controller t)
   "redirect_to(:controller => '" (s controller) "')" > p n))

(jw-template
 "redirect-to-controller-action"
 '(
   (p "Controller: " controller t)
   (p "Action: " action t)
   "redirect_to(:controller => '" (s controller) "', :action => '" (s action) "')" > p n))

(jw-template
 "redirect-to-controller-action-id"
 '(
   (p "Controller: " controller t)
   (p "Action: " action t)
   (p "ID: " id t)
   "redirect_to(:controller => '" (s controller) "', :action => '" (s action) "', :id => " (s id) ")" > p n))

;;; Controller Classes

(jw-template
 "controller-simple"
 '(
   "class " (p "Controller Base Name: " name) "Controller < ApplicationController" n
   > p n
   "end" n))

(jw-template
 "controller-scaffold"
 '(
   (P "Controller Base Name: " name t)
   (progn
     (tempo-save-named 'lcname (rails-underscore (tempo-lookup-named 'name)))
     (tempo-save-named 'pluralname (rails-pluralize (tempo-lookup-named 'lcname)))
     nil)
   "class " (s name) "Controller < ApplicationController" n
   "  def index" n
   "    list" n
   "    render :action => 'list'" n
   "  end" n
   n
   "  def list" n
   "    @" (s lcname) "_pages, @" (s pluralname) " = paginate :" (s pluralname) ", :per_page => 10" n
   "  end" n
   n
   "  def show" n
   "    @" (s lcname) " = " (s name) ".find(params[:id])" n
   "  end" n
   n
   "  def new" n
   "    @" (s lcname) " = " (s name) ".new" n
   "  end" n
   n
   "  def create" n
   "    @" (s lcname) " = " (s name) ".new(params[:" (s lcname) "])" n
   "    if @" (s lcname) ".save" n
   "      flash[:notice] = '" (s name) " was successfully created.'" n
   "      redirect_to :action => 'list'" n
   "    else" n
   "      render :action => 'new'" n
   "    end" n
   "  end" n
   n
   "  def edit" n
   "    @" (s lcname) " = " (s name) ".find(params[:id])" n
   "  end" n
   n
   "  def update" n
   "    @" (s lcname) " = " (s name) ".find(params[:id])" n
   "    if @" (s lcname) ".update_attributes(params[:" (s lcname) "])" n
   "      flash[:notice] = '" (s name) " was successfully updated.'" n
   "      redirect_to :action => 'show', :id => @" (s lcname) n
   "    else" n
   "      render :action => 'edit'" n
   "    end" n
   "  end" n
   n
   "  def destroy" n
   "    " (s name) ".find(params[:id]).destroy" n
   "    redirect_to :action => 'list'" n
   "  end" n
   "end" n ))

;;; Rails Migrations =================================================

(jw-template
 "migration-file"
 '(
   "class CreateInitial < ActiveRecord::Migration" n
   "  def self.up" n
   "    " p n
   "  end" n
   n
   "  def self.down" n
   p
   "  end" n
   "end" n
   )
 )

(jw-template
 "migration-table"
 '(
   "create_table :" > (p "Table name: ") " do |t|" > n
   > p n
   "end" >
   )
 )

(jw-template
 "migration-column-general"
 '(
   "t.column :" > (p "Column name: ") ", :" (p "Type: ")
   )
 )

(jw-template
 "migration-column-string"
 '(
   "t.column :" > (p "Column name ") ", :string, :limit=>" (p "Limit: ") ", :null=>false, :default=>''"
   )
 )

(jw-template
 "migration-column-integer"
 '(
   "t.column :" > (p "Column name ") ", :integer, :null=>false, :default=>0"
   )
 )

(jw-template
 "migration-column-key"
 '(
   "t.column :" > (p "Key name: ") ", :integer"
   )
 )

(jw-template
 "migration-add-column"
 '(
    "add_column :" (p "Table name: ") ", :" (p "Column name: ") ", :" (p "Type: ")
    )
 )
 

(defun jw-drop-tables ()
  (save-excursion
    
    )
  "hi"
  )

(jw-template
 "drop-tables"
 '((jw-create-drop-tables))
 )

;;; Validations ======================================================

(jw-template
 "validates-acceptance-of"
 '( > "validates_acceptance_of :" (p "Attribute: :") 
      ", :message => \"" (p "Message: ") "\""
      )
 )

(jw-template
 "validates-associated"
 '( > "validates_associated :" (p "Attribute: :") ))

(jw-template
 "validates-confirmation-of"
 '( > "validates_confirmation_of :" (p "Attribute: :") 
      ", :message => \"" (p "Message: ")
      ", :if => " (p "When: ")
      )
 )

(jw-template
 "validates-each"
 '(
   > "validates_each(" (p "Attributes: ") ").each do |record, attr, value|" n
     >  p n
     "end" > n
     )
 )

(jw-template
 "validates-exclusion-of"
 '( > "validates_exclustion_of :" (p "Attribute: :")
      ", :in => " (p "List: ")
      ", :message => \"" (p "Message: ") "\""
      )
 )

(jw-template
 "validates-format-of"
 '( > "validates_format_of :" (p "Attribute: :")
      ", :with => /" (p "Regex: /") "/"
      ", :message => \"" (p "Message: ") "\""
      )
 )

(jw-template
 "validates-inclusion-of"
 '( > "validates_inclusion_of :" (p "Attribute: :")
      ", :in => " (p "list: ") 
      ", :message => \"" (p "Message: ") "\""
      )
 )

(jw-template
 "validates-length-of-exact"
 '( > "validates_length_of :" (p "Attribute: :")
      ", :is => " (p "Exact length: ") 
      ", :message => \"" (p "Message: ") "\""
      )
 )

(jw-template
 "validates-length-of-max"
 '( > "validates_length_of :" (p "Attribute: :")
      ", :maximum => " (p "Max length: ") 
      ", :message => \"" (p "Message: ") "\""
      )
 )

(jw-template
 "validates-length-of-min"
 '( > "validates_length_of :" (p "Attribute: :")
      ", :minimum => " (p "Min length: ") 
      ", :message => \"" (p "Message: ") "\""
      )
 )

(jw-template
 "validates-length-of-range"
 '( > "validates_length_of :" (p "Attribute: :")
      ", :minimum => " (p "Min length: ") 
      ", :maximum => " (p "Max length: ") 
      ", :message => \"" (p "Message: ") "\""
      )
 )

(jw-template
 "validates-numericality-of"
 '( > "validates_numericality_of :" (p "Attribute: :")
      ", :only_integer => " (p "Integers only?: ")
      ", :message => \"" (p "Message: ") "\""
      )
 )

(jw-template
 "validates-presence-of"
 '( > "validates_presence_of :" (p "Attribute: :")
      ", :message => \"" (p "Message: ") "\""
      )
 )

(jw-template
 "validates-uniqueness-of"
 '( > "validates_uniqueness_of :" (p "Attribute: :")
      ", :scope => " (p "Scope (list of column names): ")
      ", :message => \"" (p "Message: ") "\""
      )
 )

;;; Done =============================================================

(provide 'rails-templates)
