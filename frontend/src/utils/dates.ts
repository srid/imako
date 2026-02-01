/**
 * Date formatting and styling utilities
 */

export const formatDate = (dateStr: string): string => {
  const date = new Date(dateStr);
  return date.toLocaleDateString("en-US", { month: "short", day: "numeric" });
};

export const dateColor = (date: string, today: string): string => {
  if (date < today) return "text-red-600 dark:text-red-400 font-medium"; // Overdue
  if (date === today) return "text-amber-600 dark:text-amber-400 font-medium"; // Today
  return "text-gray-500 dark:text-gray-400"; // Future
};
