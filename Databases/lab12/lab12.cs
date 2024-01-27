using System;
using System.Configuration;
using System.Data;
using System.Data.SqlClient;
 
namespace lab12
{
    class ConnectedLayer
    {
        private static SqlConnection connection;
        
        private static string UpdateString = "UPDATE Client SET surname = @surname WHERE email = @email";
        private static string InsertString =  "INSERT INTO Client(email, name, surname) VALUES (@email,@name,@surname); SELECT SCOPE_IDENTITY()";
        private static string DeleteString = "DELETE FROM Client WHERE id = @ClientID";
        
        
        public static void ShowTable(string tableName)
        {
            try
            {
                connection = new SqlConnection(ConfigurationManager.AppSettings.Get("connectionString"));
                connection.Open();
                Console.WriteLine("ConnectedLayer.ShowTable({0})",tableName);
                SqlCommand newComm = connection.CreateCommand();
                newComm.Connection = connection;
                newComm.CommandText = "SELECT * FROM " + tableName;

                SqlDataReader reader = newComm.ExecuteReader();

                for (int i = 0; i < reader.FieldCount; i++)
                {
                    Console.Write(reader.GetName(i) + "\t");
                }
                Console.Write("\n");
                while (reader.Read())
                {
                    for (int i = 0; i < reader.FieldCount; i++)
                    {
                        Object temp = reader.GetValue(i);
                        if (temp.ToString() != "")
                        {
                            Console.Write(temp + "\t");
                        }
                        else
                        {
                            Console.Write("NULL" + "\t");
                        }
                    }
                    Console.WriteLine();
                }

                reader.Close();
                connection.Close();
            }
            catch (Exception ex)
            {
                Console.WriteLine(ex.Message);
            }
        }
        
        public static void DeleteClientById(int id)
        {
            Console.WriteLine("ConnectedLayer.Delete({0})",id);
            try
            {
                connection = new SqlConnection(ConfigurationManager.AppSettings.Get("connectionString"));
                connection.Open();
                SqlCommand newComm = connection.CreateCommand();
                newComm.Connection = connection;
                newComm.CommandText = DeleteString;

                SqlParameter param = new SqlParameter();
                param.ParameterName = "@ClientID";
                param.Value = id;
                param.SqlDbType = SqlDbType.Int;
                
                newComm.ExecuteNonQuery();
                connection.Close();
            }
            catch (Exception ex)
            {
                Console.Write(ex.Message);
            }
        }
        
        public static void InsertClient(string email, string name, string surname)
        {
            Console.WriteLine("ConnectedLayer.Insert({0},{1},{2})",email,name,surname);
            try
            {
                connection = new SqlConnection(ConfigurationManager.AppSettings.Get("connectionString"));
                connection.Open();
                
                SqlCommand newComm = connection.CreateCommand();
                newComm.Connection = connection;
                newComm.CommandText = InsertString;

                SqlParameter[] parametes = new SqlParameter[3];
                parametes[0] = new SqlParameter();
                parametes[0].ParameterName = "@email";
                parametes[0].Value = email;
                parametes[0].SqlDbType = SqlDbType.VarChar;

                parametes[1] = new SqlParameter();
                parametes[1].ParameterName = "@name";
                parametes[1].Value = name;
                parametes[1].SqlDbType = SqlDbType.VarChar;

                parametes[2] = new SqlParameter();
                parametes[2].ParameterName = "@surname";
                parametes[2].Value = surname;
                parametes[2].SqlDbType = SqlDbType.VarChar;

                newComm.Parameters.AddRange(parametes);
                Console.WriteLine(newComm.ExecuteScalar());
                connection.Close();
            }
            catch (Exception ex)
            {
                Console.WriteLine(ex.Message);
            }
        }
        
        public static void UpdateClientSurname(string surname, string email)
        {
            Console.WriteLine("ConnectedLayer.Update({0},{1})",surname,email);
            try
            {
                connection = new SqlConnection(ConfigurationManager.AppSettings.Get("connectionString"));
                connection.Open();
                SqlCommand newComm = connection.CreateCommand();
                newComm.Connection = connection;
                newComm.CommandText = UpdateString;

                SqlParameter[] parametes = new SqlParameter[2];
                parametes[0] = new SqlParameter();
                parametes[0].ParameterName = "@surname";
                parametes[0].Value = surname;
                parametes[0].SqlDbType = SqlDbType.VarChar;

                parametes[1] = new SqlParameter();
                parametes[1].ParameterName = "@email";
                parametes[1].Value = email;
                parametes[1].SqlDbType = SqlDbType.VarChar;

                newComm.Parameters.AddRange(parametes);
                newComm.ExecuteNonQuery();
                connection.Close();
            }
            catch (Exception ex)
            {
                Console.WriteLine(ex.Message);
            }
        }
    }

    class DisconnectedLayer
    {
        private static SqlConnection connection;
        private static DataSet dataset;
        private static SqlDataAdapter dataAdapter;
        

        public DisconnectedLayer()
        {
            try
            {
                connection = new SqlConnection(ConfigurationManager.AppSettings.Get("connectionString"));
            
                dataset = new DataSet();
                dataAdapter = new SqlDataAdapter("SELECT * FROM Client", connection);
                dataAdapter.Fill(dataset, "Client");
                
                SqlCommand newComm = connection.CreateCommand();
                newComm.Connection = connection;
                newComm.CommandText = "UPDATE Client SET surname = @surname WHERE email = @email";

                SqlParameter[] parametes = new SqlParameter[2];
                parametes[0] = new SqlParameter();
                parametes[0].ParameterName = "@surname";
                parametes[0].SqlDbType = SqlDbType.VarChar;
                parametes[0].SourceColumn = "surname";

                parametes[1] = new SqlParameter();
                parametes[1].ParameterName = "@email";
                parametes[1].SqlDbType = SqlDbType.VarChar;
                parametes[1].SourceColumn = "email";

                newComm.Parameters.AddRange(parametes);
                dataAdapter.UpdateCommand = newComm;
                
                newComm = connection.CreateCommand();
                newComm.Connection = connection;
                newComm.CommandText = "INSERT INTO Client(email, name, surname) VALUES (@email,@name,@surname); SET @ID = SCOPE_IDENTITY();";

                parametes = new SqlParameter[4];
                parametes[0] = new SqlParameter();
                parametes[0].ParameterName = "@email";
                parametes[0].SqlDbType = SqlDbType.VarChar;
                parametes[0].SourceColumn = "email";

                parametes[1] = new SqlParameter();
                parametes[1].ParameterName = "@name";
                parametes[1].SqlDbType = SqlDbType.VarChar;
                parametes[1].SourceColumn = "name";

                parametes[2] = new SqlParameter();
                parametes[2].ParameterName = "@surname";
                parametes[2].SqlDbType = SqlDbType.VarChar;
                parametes[2].SourceColumn = "surname";
                
                parametes[3] = new SqlParameter();
                parametes[3].Direction = ParameterDirection.Output;
                parametes[3].ParameterName = "@ID";
                parametes[3].SqlDbType = SqlDbType.Int;
                parametes[3].SourceColumn = "id";

                newComm.Parameters.AddRange(parametes);
                dataAdapter.InsertCommand = newComm;
                dataAdapter.InsertCommand.Connection = new(ConfigurationManager.AppSettings.Get("connectionString"));
                
                newComm = new SqlCommand();
                newComm.Connection = connection;
                newComm.CommandText = "DELETE FROM Client WHERE id = @ClientID";
                
                SqlParameter param = new SqlParameter();
                param.ParameterName = "@ClientID";
                param.SqlDbType = SqlDbType.Int;
                param.SourceColumn = "id";
                
                newComm.Parameters.Add(param);
                dataAdapter.DeleteCommand = newComm;
                
            }
            catch (Exception ex)
            {
                Console.WriteLine(ex.Message);
            }
        }
        
        public void ShowTable(string tableName)
        {
            Console.WriteLine("DisconnectedLayer.ShowTable({0})",tableName);
            try
            {
                DataTableReader tableReader = dataset.CreateDataReader();
                for (int i = 0; i < tableReader.FieldCount; i++)
                {
                    Console.Write(tableReader.GetName(i) + "\t");
                }
                Console.WriteLine();
                while (tableReader.Read())
                {
                    for (int i = 0; i < tableReader.FieldCount; i++)
                    {
                        Object temp = tableReader.GetValue(i);
                        if (temp.ToString() != "")
                        {
                            Console.Write(temp + "\t");
                        }
                        else
                        {
                            Console.Write("NULL" + "\t");
                        }
                    }
                    Console.WriteLine();
                }
                Console.WriteLine();
                tableReader.Close();
            }
            catch (Exception e)
            {
                Console.WriteLine(e);
            }
        }
        
        public void InsertClient(string email, string name, string surname)
        {
            Console.WriteLine("DisconnectedLayer.Insert({0},{1},{2})",email,name,surname);
            try
            {
                DataRow dataRow = dataset.Tables["Client"].NewRow();
                dataRow["email"] = email;
                dataRow["name"] = name;
                dataRow["surname"] = surname;
                dataset.Tables["Client"].Rows.Add(dataRow);
            }
            catch (Exception e)
            {
                Console.WriteLine(e);
            }
        }

        public void UpdateClient(string surname, string email)
        {
            Console.WriteLine("DisconnectedLayer.Update({0},{1})", surname, email);

            try
            {
                for (int i = 0; i < dataset.Tables["Client"].Rows.Count; i++)
                {
                    if ((string)dataset.Tables["Client"].Rows[i]["email"] == email)
                    {
                        dataset.Tables["Client"].Rows[i]["surname"] = surname;
                        break;
                    }
                }
            }
            catch (Exception e)
            {
                Console.WriteLine(e);
            }
        }
        
        public void DeleteClientById(int id)
        {
            Console.WriteLine("DisconnectedLayer.Delete({0})",id);

            try
            {
                for (int i = 0; i < dataset.Tables["Client"].Rows.Count ; i++)
                {
                    if ((int)dataset.Tables["Client"].Rows[i]["id"] == id)
                    {
                        dataset.Tables["Client"].Rows[i].Delete();
                        break;
                    }
                }
            }
            catch (Exception e)
            {
                Console.WriteLine(e);
            }
        }

        public void CommitChanges()
        {
            try
            {
                int num = dataAdapter.Update(dataset, "Client");
                Console.WriteLine("{0} Rows was updated", num);
            } 
            catch (Exception ex)
            {
                Console.WriteLine(ex.Message);
            }
        }
    }
    
    
    
    class Program
    {   
        static void Main(string[] args)
        {
            // ConnectedLayer
            
            // ConnectedLayer.ShowTable("Client");
            // ConnectedLayer.InsertClient("test@gmail.com","name","surname");
            // ConnectedLayer.ShowTable("Client");
            // ConnectedLayer.UpdateClientSurname("kek","test@gmail.com");
            // ConnectedLayer.ShowTable("Client");
            // ConnectedLayer.DeleteClientById(1);
            // ConnectedLayer.ShowTable("Client");
            
            // DisconnectedLayer

            DisconnectedLayer dl = new DisconnectedLayer();
            dl.ShowTable("Client");
            dl.InsertClient("lab12@gmail.com","name","surname");
            dl.InsertClient("lab1214@gmail.com","name","surname");
            dl.InsertClient("lab123@gmail.com","name","surname");
            dl.ShowTable("Client");
            // // dl.UpdateClient("kek","bebra2@gmail.com");
            // // dl.ShowTable("Client");
            // // dl.DeleteClientById(1);
            // // dl.CommitChanges();
            // // dl.DeleteClientById(2);
            // dl.CommitChanges();
            // dl.ShowTable("Client");
            // // dl.ShowTable("Client"); 
        }
    }
}