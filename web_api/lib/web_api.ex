defmodule WebApi do
  @moduledoc """
  WebApi keeps the contexts that define your domain
  and business logic.

  Contexts are also responsible for managing your data, regardless
  if it comes from the database, an external API or others.
  """

  @doc """
  Convert a string to atom.

  # Example

      iex> WebApi.parse_atom("yes?")
      {:ok, :yes?}

      iex> WebApi.parse_atom("Capital Words")
      {:ok, :"Capital Words"}

      iex> WebApi.parse_atom("LONG LONG LONG LONG LONG LONG LONG LONG LONG LONG LONG LONG LONG LONG LONG LONG LONG LONG LONG LONG LONG LONG LONG LONG LONG LONG LONG LONG LONG LONG LONG LONG LONG LONG LONG LONG LONG LONG LONG LONG LONG LONG LONG LONG LONG LONG LONG LONG LONG LONG LONG LONG LONG LONG AND LONG")
      {:error, "a system limit has been reached"}
  """
  def parse_atom(string) do
    try do
      {:ok, String.to_atom(string)}
    rescue
      e in SystemLimitError -> {:error, Exception.message(e)}
    end
  end

  @doc """
  Convert a string to floating poing number.

  # Example

      iex> WebApi.parse_float("1.00")
      {:ok, 1.0}

      iex> WebApi.parse_float("10.0")
      {:ok, 10.00}

      iex> WebApi.parse_float(" 1.0")
      {:error, "argument error"}

      iex> WebApi.parse_float("3>14")
      {:error, "argument error"}
  """
  def parse_float(string) do
    try do
      {:ok, String.to_float(string)}
    rescue
    e in ArgumentError -> {:error, Exception.message(e)}
    end
  end

  @doc """
  Convert a string to integer.

  # Example

      iex> WebApi.parse_int("5555555555555555555555555555555555555555555555")
      {:ok, 5555555555555555555555555555555555555555555555}

    iex> WebApi.parse_int("3.14")
    {:error, "argument error"}

      iex> WebApi.parse_int("hey")
      {:error, "argument error"}
  """
  def parse_int(string) do
    try do
      {:ok, String.to_integer(string)}
    rescue
      e in ArgumentError -> {:error, Exception.message(e)}
    end
  end

  defp find_env_node_users() do
    case System.fetch_env("NODE_USERS") do
      {:ok, node_users} ->
        String.to_atom(node_users);
      :error ->
        :unknown_host
    end
  end

  defp find_env_node_trades() do
    case System.fetch_env("NODE_TRADES") do
      {:ok, node_trades} ->
        String.to_atom(node_trades);
      :error ->
        :unknown_host
    end
  end

  @doc """
  Informative connections to adjacent nodes.
  """
  def check_node() do
    {:net_adm.ping(find_env_node_users()), :net_adm.ping(find_env_node_trades())}
  end

  @doc """
  NAME node_users/0
  ----
  Get environment-specific node name
  OR
  :unknown_host by default.
  """
  def node_users() do
    find_env_node_users()
  end

  @doc """
  NAME node_trades/0
  ----
  Get environment-specific node name
  OR
  :unknown_host by default.
  """
  def node_trades() do
    find_env_node_trades()
  end
end
