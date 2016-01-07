defmodule MyApp.Presenters.EmailPresenter do
  def register_template(user) do
    """
    <p><b>Hi #{user.username},</b></p>
    <p>Thanks for joining!</p>
    <p>Cheers!</p>
    <p></p>
    <p>MyApp</p>
    """
  end

  def password_recovery_template(user) do
    """
    <p><b>Hi #{user.username},</b></p>
    <p> It seems you've lost your password! </p>
    <p> Use this token <b>#{user.recovery_hash}"</b> to recover your password.</p>
    <p>
    <p>Cheers!</p>
    <p>MyApp</p>
    """
  end


end
